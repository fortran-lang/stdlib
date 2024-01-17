program example_string_to_number
    use stdlib_kinds, only: dp
    use stdlib_str2num, only: to_num
    implicit none
    character(:), allocatable :: txt
    real(dp) :: x
  
    txt = ' 8.8541878128e−12 '
    x = to_num( txt , x )
  end program example_string_to_number