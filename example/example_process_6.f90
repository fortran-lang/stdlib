! Process example 6: Demonstrate callback 
program example_process_6
    use stdlib_system, only: process_type, process_ID, run, is_running, kill, elapsed, is_windows, sleep
    implicit none
    type(process_type) :: p
    integer, target :: nfiles

    ! Run process, attach callback function and some data
    if (is_windows()) then
        p = run("dir",want_stdout=.true.,callback=get_dir_nfiles)
    else
        p = run("ls -l",want_stdout=.true.,callback=get_dir_nfiles,payload=nfiles)
    endif

    ! On exit, the number of files should have been extracted by the callback function
    print *, "Current directory has ",nfiles," files"
    
    contains
        
       ! Custom callback function: retrieve number of files from ls output
       subroutine get_dir_nfiles(pid, exit_state, stdin, stdout, stderr, payload)
           integer(process_ID), intent(in) :: pid
           integer, intent(in) :: exit_state
           character(len=*), optional, intent(in) :: stdin, stdout, stderr
           class(*), optional, intent(inout) :: payload    
           
           integer :: i

           if (present(payload)) then 
            
               select type (nfiles => payload)
                   type is (integer)
                       if (present(stdout)) then 
                           nfiles = count([ (stdout(i:i) == char(10), i=1,len(stdout)) ])
                       else
                           nfiles = -1
                       endif 
                   class default
                       error stop 'Wrong payload passed to the process'
               end select            
            
           end if
       end subroutine get_dir_nfiles     

end program example_process_6
