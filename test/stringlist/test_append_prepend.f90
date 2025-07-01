! SPDX-Identifier: MIT
module test_append_prepend
    use stdlib_error, only: check
    use stdlib_string_type, only: string_type, operator(//), operator(==)
    use stdlib_stringlist_type, only: stringlist_type, fidx, bidx, list_head, &
                            & list_tail, operator(//), operator(==), operator(/=)
    use stdlib_strings, only: to_string
    implicit none    

contains

    subroutine test_append_prepend_string
        type(stringlist_type)           :: work_list
        type(stringlist_type)           :: reference_list
        integer                         :: i
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        character(len=:), allocatable   :: string
        type(string_type)               :: all_strings(first:last)
        
        do concurrent (i=first:last)
            all_strings(i) = string_type( to_string(i) )
        end do        

        do i = first, last
            string = to_string(i)
            work_list = work_list // string
            call check( work_list%get( fidx( i - first + 1 ) ) == string_type( string ), &
                        & "test_append_prepend_string: get fidx( i - first + 1 ) " // string )

        end do

        call compare_list( work_list, first, last + 1, 1 )
        call check( work_list == all_strings, &
                        & "test_append_prepend_string: work_list ==&
                        & [ ( string_type( to_string(i) ), i = first, last ) ]" )
        call check( all_strings == work_list, &
                        & "test_append_prepend_string: [ ( string_type( to_string(i) ),&
                        & i = first, last ) ] == work_list" )

        do i = last, first, -1
            call check( work_list /= reference_list, "test_append_prepend_string:&
                                    & work_list /= reference_list" )
            call check( reference_list /= work_list, "test_append_prepend_string:&
                                    & reference_list /= work_list" )

            string = to_string(i)
            reference_list = string_type( string ) // reference_list
            call check( reference_list%get( bidx( last - i + 1 ) ) == string, &
                                    & "test_append_prepend_string: get bidx( last - i + 1 ) " // string )

        end do

        call compare_list( reference_list, first, last + 1, 2 )
        call check( reference_list == all_strings, "test_append_prepend_string:&
                    & reference_list == [ ( string_type( to_string(i) ), i = first, last ) ]" )
        call check( all_strings == reference_list, &
                    & "test_append_prepend_string: [ ( string_type( to_string(i) ), i = first, last ) ] == reference_list" )

        call check( work_list == reference_list, "test_append_prepend_string:&
                                    & work_list == reference_list" )
        call check( reference_list == work_list, "test_append_prepend_string:&
                                    & reference_list == work_list" )

    end subroutine test_append_prepend_string

    subroutine test_append_prepend_array
        type(stringlist_type)           :: work_list
        type(stringlist_type)           :: reference_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 10
        type(string_type)               :: all_strings(first:last)
        
        do concurrent (j=first:last)
            all_strings(j) = string_type( to_string(j) )
        end do   

        do i = first, last - 1, stride
            work_list = work_list // all_strings(i:i+stride-1)
            call check( work_list == all_strings(first:i+stride-1), &
                    & "test_append_prepend_array: work_list ==&
                    & [ ( string_type( to_string(j) ), j = first, i + stride - 1) ]" )

        end do

        work_list = work_list // to_string(last)

        call compare_list( work_list, first, last + 1, 3 )
        call check( work_list == all_strings, &
                    & "test_append_prepend_array: work_list ==&
                    & [ ( string_type( to_string(i) ), i = first, last) ]" )
        call check( all_strings == work_list, &
                    & "test_append_prepend_array: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == work_list" )

        do i = last, first + 1, -1 * stride
            call check( work_list /= reference_list, "test_append_prepend_array:&
                                    & work_list /= reference_list" )
            call check( reference_list /= work_list, "test_append_prepend_array:&
                                    & reference_list /= work_list" )

            reference_list = all_strings(i-stride+1:i) // reference_list
            call check( reference_list == all_strings(i-stride+1:last), &
                            & "test_append_prepend_array: reference_list ==&
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ]" )

        end do

        reference_list = to_string(first) // reference_list

        call compare_list( reference_list, first, last + 1, 4 )
        call check( all_strings == reference_list, &
                    & "test_append_prepend_array:&
                    & [ ( string_type( to_string(i) ), i = first, last) ] == reference_list" )
        call check( all_strings == reference_list, &
                    & "test_append_prepend_array: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == reference_list" )

        call check( work_list == reference_list, "test_append_prepend_array:&
                                    & work_list == reference_list" )
        call check( reference_list == work_list, "test_append_prepend_array:&
                                    & reference_list == work_list" )

    end subroutine test_append_prepend_array

    subroutine test_append_prepend_list
        type(stringlist_type)           :: work_list, reference_list
        type(stringlist_type)           :: temp_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 10
        type(string_type)               :: all_strings(first:last)
        
        do concurrent (j=first:last)
            all_strings(j) = string_type( to_string(j) )
        end do   

        do i = first, last - 1, stride
            call temp_list%clear()
            do j = i, i + stride - 1
                call temp_list%insert_at( list_tail, string_type( to_string(j) ) )
            end do
            work_list = work_list // temp_list
            
            call check( work_list == all_strings(first:i+stride-1), &
                    & "test_append_prepend_list: work_list ==&
                    & [ ( to_string(j), j = first, i + stride - 1) ]" )

        end do

        work_list = work_list // to_string(last)

        call compare_list( work_list, first, last + 1, 5 )
        call check( work_list == all_strings, "test_append_prepend_list:&
                    & work_list == [ ( string_type( to_string(i) ), i = first, last) ]" )
        call check( all_strings == work_list, &
                    & "test_append_prepend_list: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == work_list" )

        do i = last, first + 1, -1 * stride
            call check( work_list /= reference_list, "test_append_prepend_list:&
                                    & work_list /= reference_list" )
            call check( reference_list /= work_list, "test_append_prepend_list:&
                                    & reference_list /= work_list" )

            call temp_list%clear()
            do j = i - stride + 1, i
                call temp_list%insert_at( list_tail, to_string(j) )
            end do
            reference_list = temp_list // reference_list

            call check( reference_list == &
                            & all_strings(i-stride+1:last), &
                            & "test_append_prepend_list: reference_list ==&
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ]" )

        end do

        reference_list = to_string(first) // reference_list

        call compare_list( reference_list, first, last + 1, 6 )
        call check( all_strings == reference_list, &
                    & "test_append_prepend_list:&
                    & [ ( string_type( to_string(i) ), i = first, last) ] == reference_list" )
        call check( all_strings == reference_list, &
                    & "test_append_prepend_list: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == reference_list" )

        call check( work_list == reference_list, "test_append_prepend_list:&
                                    & work_list == reference_list" )
        call check( reference_list == work_list, "test_append_prepend_list:&
                                    & reference_list == work_list" )

    end subroutine test_append_prepend_list

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

end module test_append_prepend


program tester
    use test_append_prepend
    implicit none
    
    call test_append_prepend_string
    call test_append_prepend_array
    call test_append_prepend_list

end program tester
