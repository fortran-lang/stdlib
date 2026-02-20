program test_input
  ! Test program for the new input() function
  use stdlib_io, only: input
  implicit none
  
  character(len=:), allocatable :: name, age_str, city
  integer :: ios
  
  ! Test 1: Basic input with prompt
  write(*,'(a)') 'Test 1: Basic input with prompt'
  name = input('Enter your name: ')
  write(*,'(2a)') 'You entered: ', name
  write(*,*)
  
  ! Test 2: Input without prompt
  write(*,'(a)') 'Test 2: Input without prompt'
  write(*,'(a)',advance='no') 'Enter your age: '
  age_str = input()
  write(*,'(2a)') 'You entered: ', age_str
  write(*,*)
  
  ! Test 3: Input with iostat
  write(*,'(a)') 'Test 3: Input with iostat (press Ctrl+D to end)'
  city = input('Enter your city: ', iostat=ios)
  if (ios == 0) then
    write(*,'(2a)') 'You entered: ', city
  else
    write(*,'(a,i0)') 'Input error, iostat = ', ios
  end if
  
end program test_input
