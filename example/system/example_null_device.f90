! Showcase usage of the null device
program example_null_device
    use stdlib_system, only: null_device
    use iso_fortran_env, only: output_unit
    implicit none
    integer :: unit 
    logical :: screen_output = .false.

    if (screen_output) then 
       unit = output_unit
    else
       ! Write to the null device if no screen output is wanted
       open(newunit=unit,file=null_device())
    endif     

    write(unit,*) "Hello, world!" 

    if (.not.screen_output) close(unit) 

end program example_null_device
