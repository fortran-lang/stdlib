program example_unique
    use stdlib_kinds, only: dp, sp
    use stdlib_sorting, only: unique
    use stdlib_string_type, only: string_type
    implicit none
    
    ! Example with integer array
    integer :: int_array(10) = [1, 2, 3, 3, 4, 5, 5, 6, 6, 6]
    integer, allocatable :: int_unique(:)
    
    ! Example with real array
    real(sp) :: real_array(8) = [3.1, 2.5, 7.2, 3.1, 2.5, 8.0, 7.2, 9.5]
    real(sp), allocatable :: real_unique(:)
    
    ! Example with character array
    character(len=6) :: char_array(7) = ["apple ", "banana", "cherry", "apple ", "date  ", "banana", "cherry"]
    character(len=6), allocatable :: char_unique(:)
    
    ! Example with string_type array
    type(string_type) :: string_array(8), string_unique_sorted(4)
    type(string_type), allocatable :: string_unique(:)
    
    integer :: i
    
    ! Setup string array
    string_array(1) = "apple"
    string_array(2) = "banana"
    string_array(3) = "cherry" 
    string_array(4) = "apple"
    string_array(5) = "date"
    string_array(6) = "banana"
    string_array(7) = "cherry"
    string_array(8) = "apple"
    
    ! Get unique integer values
    int_unique = unique(int_array)
    print *, "Unique integers:", int_unique
    
    ! Get sorted unique integer values
    int_unique = unique(int_array, sorted=.true.)
    print *, "Sorted unique integers:", int_unique
    
    ! Get unique real values
    real_unique = unique(real_array)
    print *, "Unique reals:", real_unique
    
    ! Get sorted unique real values
    real_unique = unique(real_array, sorted=.true.)
    print *, "Sorted unique reals:", real_unique
    
    ! Get unique character values
    char_unique = unique(char_array)
    print *, "Unique strings:"
    do i = 1, size(char_unique)
        print *, char_unique(i)
    end do
    
    ! Get unique string_type values (sorted)
    string_unique = unique(string_array, sorted=.true.)
    print *, "Sorted unique string_type values:"
    do i = 1, size(string_unique)
        print *, string_unique(i)
    end do
end program example_unique 