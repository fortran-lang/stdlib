module fortran_subprocess
    use iso_c_binding  
    use iso_fortran_env, only: int64, real64
    use stdlib_system
    use stdlib_io, only: getfile
    use stdlib_strings, only: to_c_string
    implicit none
    public
    
    ! Interoperable types    
    integer, parameter, public :: pid_t = c_int64_t
    
    logical(c_bool), parameter, private :: C_FALSE = .false._c_bool
    logical(c_bool), parameter, private :: C_TRUE  = .true._c_bool
    
    ! CPU clock ticks range
    integer, parameter, private ::  TICKS = int64
    integer, parameter, private :: RTICKS = real64
    
    ! Number of CPU ticks between status updates
    integer(TICKS), parameter :: CHECK_EVERY_TICKS = 100
    
    ! Default flag for the runner process
    integer(pid_t), parameter :: FORKED_PROCESS = 0_pid_t
    
    ! Interface to C support functions from stdlib_system_subprocess.c
    interface
        
        ! C wrapper to query process status
        subroutine process_query_status(pid, wait, is_running, exit_code) &
                   bind(C, name='process_query_status')
            import c_int, c_bool, pid_t
            implicit none
            ! Process ID
            integer(pid_t), value :: pid       
            ! Whether to wait for process completion      
            logical(c_bool), value :: wait     
            ! Whether the process is still running
            logical(c_bool), intent(out) :: is_running  
            ! Process exit code (or error code)
            integer(c_int), intent(out) :: exit_code     
        end subroutine process_query_status

        subroutine process_create(cmd, stdin_stream, stdin_file, stdout_file, stderr_file, handle, pid) &
                   bind(C, name='process_create')
            import c_char, c_ptr, pid_t
            implicit none
            character(c_char), intent(in)           :: cmd(*)
            character(c_char), intent(in), optional :: stdin_stream(*)
            character(c_char), intent(in), optional :: stdin_file(*)
            character(c_char), intent(in), optional :: stdout_file(*)
            character(c_char), intent(in), optional :: stderr_file(*)
            type(c_ptr)      , intent(out)          :: handle
            integer(pid_t),    intent(out)          :: pid
        end subroutine process_create
        
        subroutine process_wait(seconds) bind(C,name='process_wait')
            import c_float
            implicit none
            real(c_float), intent(in) :: seconds
        end subroutine process_wait
  
    end interface

    type, public :: process_type
        
        !> Process ID (if external); 0 if run by the program process
        integer(pid_t) :: id = FORKED_PROCESS
        type(c_ptr) :: handle = c_null_ptr
        
        !> Process is completed
        logical :: completed = .false.        
        integer(TICKS) :: start_time = 0
        
        !> Process exit code
        integer :: exit_code = 0
        
        !> Stdin file name
        character(:), allocatable :: stdin_file
        
        !> Standard output
        character(:), allocatable :: stdout_file
        character(:), allocatable :: stdout
        
        !> Error output
        character(:), allocatable :: stderr_file
        character(:), allocatable :: stderr
        
        !> Store time at the last update
        integer(TICKS) :: last_update = 0
        
        contains
        
           !> Return process lifetime so far, in seconds
           procedure :: elapsed => process_lifetime
           
           !> Live check if a process is still running
           procedure :: is_running   => process_is_running
           procedure :: is_completed => process_is_completed
           
           !> Wait until a running process is completed
           procedure :: wait => wait_for_completion
            
    end type process_type
    
  
contains

    !> Open a new, asynchronous process
    type(process_type) function process_open(args,wait,stdin,want_stdout,want_stderr) result(process)
        !> The command and arguments
        character(*), intent(in) :: args(:)
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, optional, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr
        
        real(RTICKS) :: count_rate
        logical :: asynchronous, collect_stdout, collect_stderr, has_stdin
        integer :: command_state, exit_state
        integer(TICKS) :: count_max
        
        ! Process user requests
        asynchronous   = .false.
        collect_stdout = .false.
        collect_stderr = .false.
        has_stdin      = present(stdin)
        if (present(wait))        asynchronous   = .not.wait
        if (present(want_stdout)) collect_stdout = want_stdout
        if (present(want_stderr)) collect_stderr = want_stderr
        
        ! Attach stdout to a scratch file (must be named)
        if (has_stdin)      process%stdin_file  = scratch_name('inp')
        if (collect_stdout) process%stdout_file = scratch_name('out')            
        if (collect_stderr) process%stderr_file = scratch_name('err')
        
        ! Save the process's generation time
        call system_clock(process%start_time,count_rate,count_max)
        process%last_update = process%start_time                
        
        if (asynchronous) then 
            
           ! Create or fork a new process, store pid 
           call launch_asynchronous(process, args, stdin)
           
        else

           ! No need to create an external process
           process%id = FORKED_PROCESS

        endif
        
        if (process%id == FORKED_PROCESS) then 
           
           ! Launch to completion from the local process
           call launch_synchronous(process, args, stdin)
           call save_completed_state(process,delete_files=.not.asynchronous)

           ! If the process was forked 
           ! Note: use `exit` rather than `stop` to prevent the mandatory stdout STOP message           
           if (asynchronous) then 
               if (command_state/=0) then 
                   ! Invalid command: didn't even start
                   call exit(command_state)
               else
                   ! Return exit state
                   call exit(exit_state)                   
               end if                   
           endif            
           
        endif
              

           
        ! Run a first update
        call update_process_state(process)   
           
    end function process_open
    
    subroutine launch_asynchronous(process, args, stdin)
        class(process_type), intent(inout) :: process
        !> The command and arguments
        character(*), intent(in) :: args(:)        
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin  
        
        character(c_char), dimension(:), allocatable, target :: c_cmd,c_stdin,c_stdin_file,c_stdout_file,c_stderr_file
        
        ! Assemble C strings 
                                            c_cmd   = c_string(join(args))
        if (present(stdin))                 c_stdin = c_string(stdin)
        if (allocated(process%stdin_file))  c_stdin_file  = c_string(process%stdin_file)
        if (allocated(process%stdout_file)) c_stdout_file = c_string(process%stdout_file)
        if (allocated(process%stderr_file)) c_stderr_file = c_string(process%stderr_file)
            
        ! On Windows, this 1) creates 2) launches an external process from C.
        ! On unix, this 1) forks an external process
        call process_create(c_cmd, c_stdin, c_stdin_file, c_stdout_file, c_stderr_file, process%handle, process%id)
        
    end subroutine launch_asynchronous
    
    subroutine launch_synchronous(process, args, stdin)        
        class(process_type), intent(inout) :: process
        !> The command and arguments
        character(*), intent(in) :: args(:)        
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin        
        
        character(:), allocatable :: cmd
        character(4096) :: iomsg
        integer :: iostat,estat,cstat,stdin_unit
        logical :: has_stdin
        
        has_stdin      = present(stdin)
        
        ! Prepare stdin
        if (has_stdin) then 
            
            open(newunit=stdin_unit,file=process%stdin_file, &
                 access='stream',action='write',position='rewind', &
                 iostat=iostat,iomsg=iomsg)                      
            if (iostat/=0) error stop 'cannot open temporary stdin'               
               
            write(stdin_unit,iostat=iostat,iomsg=iomsg) stdin               
            if (iostat/=0) error stop trim(iomsg)
               
            close(stdin_unit,iostat=iostat,iomsg=iomsg,status='keep')
            if (iostat/=0) error stop 'cannot close temporary stdin'
               
        end if
                       
        ! Run command
        cmd = assemble_cmd(args,process%stdin_file,process%stdout_file,process%stderr_file)
           
        ! Execute command        
        call execute_command_line(cmd,wait=.true.,exitstat=estat,cmdstat=cstat)         
        
        ! Save state and output
        process%exit_code = merge(cstat,estat,cstat/=0)        
        
    end subroutine launch_synchronous
    
    !> Return the current (or total) process lifetime, in seconds
    real(RTICKS) function process_lifetime(process) result(delta_t)
        class(process_type), intent(in) :: process 
        
        real(RTICKS) :: ticks_per_second
        integer(TICKS) :: current_time,count_max
        
        ! Get current time
        call system_clock(current_time,ticks_per_second,count_max)        
                
        if (process%completed) then 
            
            delta_t = real(process%last_update-process%start_time,RTICKS)/ticks_per_second
            
        else
            
            delta_t = real(current_time-process%start_time,RTICKS)/ticks_per_second
            
        end if
        
    end function process_lifetime
    
    !> Wait for a process to be completed
    subroutine wait_for_completion(process, max_wait_time)
        class(process_type), intent(inout) :: process
        ! Optional max wait time in seconds
        real, optional, intent(in) :: max_wait_time

        real(RTICKS) :: wait_time, elapsed
        integer(TICKS) :: start_time, current_time, count_rate

        ! Determine the wait time
        if (present(max_wait_time)) then
            wait_time = max(0.0_RTICKS, max_wait_time)
        else
            ! No limit if max_wait_time is not provided
            wait_time = huge(wait_time)  
        end if

        ! Get the system clock rate and the start time
        call system_clock(start_time, count_rate)
        elapsed = 0.0_real64

        ! Wait loop
        wait_loop: do while (process_is_running(process) .and. elapsed <= wait_time)
            
            ! Small sleep to avoid CPU hogging (1 ms)
            call process_wait(0.001_c_float)
            
            call system_clock(current_time)
            elapsed = real(current_time - start_time, RTICKS) / count_rate
            
        end do wait_loop

    end subroutine wait_for_completion

    !> Update a process's state, and 
    subroutine update_process_state(process)
        class(process_type), intent(inout) :: process
        
        real(RTICKS) :: count_rate        
        integer(TICKS) :: count_max,current_time
        logical(c_bool) :: is_running
        integer(c_int) :: exit_code
        
        ! If the process has completed, should not be queried again
        if (process%completed) return
        
        ! Save the process's generation time
        call system_clock(current_time,count_rate,count_max)
        
        ! Only trigger an update after at least 100 count units
        if (abs(real(current_time-process%last_update,RTICKS))<CHECK_EVERY_TICKS) return
        
        ! Update check time
        process%last_update = current_time       
        
        if (process%id /= FORKED_PROCESS) then 
        
            ! Query process state
            call process_query_status(process%id, wait=C_FALSE, is_running=is_running, exit_code=exit_code)
            
            process%completed = .not.is_running
            
            if (process%completed) then 
               ! Process completed, may have returned an error code  
               process%exit_code = exit_code
               call save_completed_state(process,delete_files=.true.)                 
            end if
        
        endif
        
    end subroutine update_process_state
    
    subroutine save_completed_state(process,delete_files)
        type(process_type), intent(inout) :: process
        logical, intent(in) :: delete_files
        
        logical(c_bool) :: is_running        
        integer(c_int) :: exit_code        
        integer :: delete
        
        ! Same as process ID: process exited
        process%completed = .true.
        
        ! Clean up process state using waitpid
        if (process%id/=FORKED_PROCESS) call process_query_status(process%id, C_TRUE, is_running, exit_code)
       
        ! Process is over: load stdout/stderr if requested
        if (allocated(process%stderr_file)) then 
           process%stderr = getfile(process%stderr_file,delete=delete_files)
           deallocate(process%stderr_file)
        endif
    
        if (allocated(process%stdout_file)) then 
           process%stdout = getfile(process%stdout_file,delete=delete_files)
           deallocate(process%stdout_file)
        endif         
        
        if (allocated(process%stdin_file)) then 
            open(newunit=delete,file=process%stdin_file,access='stream',action='write')
            close(delete,status='delete')
            deallocate(process%stdin_file)
        end if
        
    end subroutine save_completed_state

    !> Live check if a process is running
    logical function process_is_running(process) result(is_running)
        class(process_type), intent(inout) :: process
        
        ! Each evaluation triggers a state update
        call update_process_state(process)

        is_running = .not.process%completed
        
    end function process_is_running
    
    !> Live check if a process has completed
    logical function process_is_completed(process) result(is_completed)
        class(process_type), intent(inout) :: process
        
        ! Each evaluation triggers a state update
        call update_process_state(process)

        is_completed = process%completed
        
    end function process_is_completed    
            
    function scratch_name(prefix) result(temp_filename)
        character(*), optional, intent(in) :: prefix
        character(:), allocatable :: temp_filename
        character(len=8)  :: date
        character(len=10) :: time
        character(len=7)  :: rand_str
        real :: rrand
        integer :: rand_val

        ! Get the current date and time
        call date_and_time(date=date, time=time)

        ! Generate a random number for additional uniqueness
        call random_number(rrand)
        rand_val = nint(rrand * 1e6)  ! Scale random number
        write(rand_str,'(i7.7)') rand_val
        
        ! Construct the filename
        if (present(prefix)) then 
           temp_filename = trim(prefix)// '_' // date // '_' // time(1:6) // '_' // rand_str // '.tmp'
        else
           temp_filename = 'tmp_' // date // '_' // time(1:6) // '_' // rand_str // '.tmp'
        endif
        
    end function scratch_name
        

end module fortran_subprocess
