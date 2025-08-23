submodule (stdlib_system) stdlib_system_subprocess
    use iso_c_binding  
    use iso_fortran_env, only: int64, real64
    use stdlib_strings, only: join
    use stdlib_linalg_state, only: linalg_state_type, LINALG_ERROR, linalg_error_handling
    implicit none(type, external)
    
    ! Number of CPU ticks between status updates
    integer(TICKS), parameter :: CHECK_EVERY_TICKS = 100
    
    ! Interface to C support functions from stdlib_system_subprocess.c
    interface
        
        ! C wrapper to query process status
        subroutine process_query_status(pid, wait, is_running, exit_code) &
                   bind(C, name='process_query_status')
            import c_int, c_bool, process_ID
            implicit none
            ! Process ID
            integer(process_ID), value :: pid       
            ! Whether to wait for process completion      
            logical(c_bool), value :: wait     
            ! Whether the process is still running
            logical(c_bool), intent(out) :: is_running  
            ! Process exit code (or error code)
            integer(c_int), intent(out) :: exit_code     
        end subroutine process_query_status

        subroutine process_create(cmd, stdin_stream, stdin_file, stdout_file, stderr_file, pid) &
                   bind(C, name='process_create')
            import c_char, process_ID
            implicit none
            character(c_char), intent(in)           :: cmd(*)
            character(c_char), intent(in), optional :: stdin_stream(*)
            character(c_char), intent(in), optional :: stdin_file(*)
            character(c_char), intent(in), optional :: stdout_file(*)
            character(c_char), intent(in), optional :: stderr_file(*)
            integer(process_ID), intent(out)        :: pid
        end subroutine process_create
        
        logical(c_bool) function process_system_kill(pid) bind(C, name='process_kill')
            import c_bool, process_ID
            implicit none
            integer(process_ID), intent(in), value :: pid
        end function process_system_kill
        
        ! System implementation of a wait function
        subroutine process_wait(seconds) bind(C,name='process_wait')
            import c_float
            implicit none
            real(c_float), intent(in), value :: seconds
        end subroutine process_wait            
        
        ! Utility: check if _WIN32 is defined in the C compiler
        logical(c_bool) function process_is_windows() bind(C,name='process_is_windows')
            import c_bool
            implicit none
        end function process_is_windows
  
    end interface

    ! C boolean constants
    logical(c_bool), parameter :: C_FALSE = .false._c_bool
    logical(c_bool), parameter :: C_TRUE  = .true._c_bool

contains

    ! Call system-dependent wait implementation
    module subroutine sleep(millisec)
        integer, intent(in) :: millisec
        
        real(c_float) :: seconds
        
        seconds = 0.001_c_float*max(0,millisec)
                        
        call process_wait(seconds)
        
    end subroutine sleep

    module function run_async_cmd(cmd, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> The command line string to execute.
        character(*), intent(in) :: cmd
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler.
        type(process_type) :: process
        
        process = process_open([cmd],.false.,stdin,want_stdout,want_stderr,callback,payload)
        
    end function run_async_cmd

    module function run_async_args(args, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> List of arguments for the process to execute.
        character(*), intent(in) :: args(:)
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler.
        type(process_type) :: process  
        
        process = process_open(args,.false.,stdin,want_stdout,want_stderr,callback,payload)
              
    end function run_async_args

    module function run_sync_cmd(cmd, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> The command line string to execute.
        character(*), intent(in) :: cmd
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload                
        !> The output process handler.
        type(process_type) :: process
        
        process = process_open([cmd],.true.,stdin,want_stdout,want_stderr,callback,payload)
        
    end function run_sync_cmd

    module function run_sync_args(args, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> List of arguments for the process to execute.
        character(*), intent(in) :: args(:)
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler.
        type(process_type) :: process  
        
        process = process_open(args,.true.,stdin,want_stdout,want_stderr,callback,payload)
              
    end function run_sync_args

    !> Internal function: open a new process from a command line
    function process_open_cmd(cmd,wait,stdin,want_stdout,want_stderr,callback,payload) result(process)
        !> The command and arguments
        character(*), intent(in) :: cmd
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler
        type(process_type) :: process        
        
        process = process_open([cmd],wait,stdin,want_stdout,want_stderr,callback,payload)
        
    end function process_open_cmd

    !> Internal function: open a new process from arguments
    function process_open(args,wait,stdin,want_stdout,want_stderr,callback,payload) result(process)
        !> The command and arguments
        character(*), intent(in) :: args(:)
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler
        type(process_type) :: process
        
        real(RTICKS) :: count_rate
        logical :: asynchronous, collect_stdout, collect_stderr, has_stdin
        integer :: command_state, exit_state
        integer(TICKS) :: count_max
        
        ! Process user requests
        asynchronous   = .not.wait
        collect_stdout = .false.
        collect_stderr = .false.
        has_stdin      = present(stdin)
        if (present(want_stdout)) collect_stdout = want_stdout
        if (present(want_stderr)) collect_stderr = want_stderr
        
        ! Attach stdout to a scratch file (must be named)
        if (has_stdin)      process%stdin_file  = scratch_name('inp')
        if (collect_stdout) process%stdout_file = scratch_name('out')            
        if (collect_stderr) process%stderr_file = scratch_name('err')
        
        ! Attach callback function and payload
        if (present(callback)) then 
            process%oncomplete => callback
        else
            nullify(process%oncomplete)
        end if
        
        if (present(payload)) then 
            process%payload => payload
        else
            nullify(process%payload)            
        end if
        
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
                                            c_cmd   = to_c_char(join(args))
        if (present(stdin))                 c_stdin = to_c_char(stdin)
        if (allocated(process%stdin_file))  c_stdin_file  = to_c_char(process%stdin_file)
        if (allocated(process%stdout_file)) c_stdout_file = to_c_char(process%stdout_file)
        if (allocated(process%stderr_file)) c_stderr_file = to_c_char(process%stderr_file)
            
        ! On Windows, this 1) creates 2) launches an external process from C.
        ! On unix, this 1) forks an external process
        call process_create(c_cmd, c_stdin, c_stdin_file, c_stdout_file, c_stderr_file, process%id)
        
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
    module real(RTICKS) function process_lifetime(process) result(delta_t)
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
    module subroutine wait_for_completion(process, max_wait_time)
        class(process_type), intent(inout) :: process
        ! Optional max wait time in seconds
        real, optional, intent(in) :: max_wait_time

        integer :: sleep_interval
        real(RTICKS) :: wait_time, elapsed
        integer(TICKS) :: start_time, current_time, count_rate
        
        ! Sleep interval ms
        integer, parameter :: MIN_WAIT_MS = 1
        integer, parameter :: MAX_WAIT_MS = 100
        
        ! Starting sleep interval: 1ms
        sleep_interval = MIN_WAIT_MS

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
        
            ! Small sleep to avoid CPU hogging, with exponential backoff  (1 ms)
            ! from 1ms up to 100ms
            call sleep(millisec=sleep_interval)
            sleep_interval = min(sleep_interval*2, MAX_WAIT_MS)
            
            call system_clock(current_time)
            elapsed = real(current_time - start_time, RTICKS) / count_rate
            
        end do wait_loop

    end subroutine wait_for_completion

    !> Update a process's state, and save it to the process variable
    module subroutine update_process_state(process)
        class(process_type), intent(inout) :: process
        
        real(RTICKS) :: count_rate        
        integer(TICKS) :: count_max,current_time
        logical(c_bool) :: running
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
            call process_query_status(process%id, wait=C_FALSE, is_running=running, exit_code=exit_code)
            
            process%completed = .not.running
            
            if (process%completed) then 
               ! Process completed, may have returned an error code  
               process%exit_code = exit_code
               call save_completed_state(process,delete_files=.true.)                 
            end if
        
        endif
        
    end subroutine update_process_state
    
    ! Kill a process 
    module subroutine process_kill(process, success)
        class(process_type), intent(inout) :: process
        ! Return a boolean flag for successful operation
        logical, intent(out) :: success
        
        integer(c_int) :: exit_code
        logical(c_bool) :: running
        
        success = .true.
        
        ! No need to
        if (process%completed) return
        if (process%id == FORKED_PROCESS) return
        
        success = logical(process_system_kill(process%id))
        
        if (success) then 
            
            call process_query_status(process%id, wait=C_TRUE, is_running=running, exit_code=exit_code)
            
            process%completed = .not.running
            
            if (process%completed) then 
               ! Process completed, may have returned an error code  
               process%exit_code = exit_code
               call save_completed_state(process,delete_files=.true.)                 
            end if            
            
        end if
        
    end subroutine process_kill
    
    subroutine save_completed_state(process,delete_files)
        class(process_type), intent(inout) :: process
        logical, intent(in) :: delete_files
        
        logical(c_bool) :: running        
        integer(c_int) :: exit_code        
        integer :: delete
        
        ! Same as process ID: process exited
        process%completed = .true.
        
        ! Clean up process state using waitpid
        if (process%id/=FORKED_PROCESS) call process_query_status(process%id, C_TRUE, running, exit_code)
       
        ! Process is over: load stderr if requested
        if (allocated(process%stderr_file)) then 
           process%stderr = getfile(process%stderr_file,delete=delete_files)
           deallocate(process%stderr_file)
        endif
    
        ! Process is over: load stdout if requested
        if (allocated(process%stdout_file)) then 
           process%stdout = getfile(process%stdout_file,delete=delete_files)
           deallocate(process%stdout_file)
        endif         
        
        ! Process is over: delete stdin file if it was provided
        if (allocated(process%stdin_file)) then 
            process%stdin = getfile(process%stdin_file,delete=delete_files)
            deallocate(process%stdin_file)
        end if
        
        ! Process is over: invoke callback if requested
        if (associated(process%oncomplete)) &
            call process%oncomplete(process%id,        &
                                    process%exit_code, &
                                    process%stderr,    &
                                    process%stdout,    &
                                    process%stderr,    &
                                    process%payload)
        
    end subroutine save_completed_state

    !> Live check if a process is running
    module logical function process_is_running(process) result(is_running)
        class(process_type), intent(inout) :: process
        
        ! Each evaluation triggers a state update
        call update_process_state(process)

        is_running = .not.process%completed
        
    end function process_is_running
    
    !> Live check if a process has completed
    module logical function process_is_completed(process) result(is_completed)
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
        
        
    !> Assemble a single-line proces command line from a list of arguments.
    !> 
    !> Version: Helper function.
    function assemble_cmd(args, stdin, stdout, stderr) result(cmd)
        !> Command to execute as a string
        character(len=*), intent(in) :: args(:)    
        !> [optional] File name standard input (stdin) should be taken from
        character(len=*), optional, intent(in) :: stdin    
        !> [optional] File name standard output (stdout) should be directed to
        character(len=*), optional, intent(in) :: stdout
        !> [optional] File name error output (stderr) should be directed to
        character(len=*), optional, intent(in) :: stderr
        
        character(:), allocatable :: cmd,stdout_file,input_file,stderr_file
        
        if (present(stdin)) then 
            input_file = stdin
        else
            input_file = null_device()        
        end if

        if (present(stdout)) then
            ! Redirect output to a file
            stdout_file = stdout
        else
            stdout_file = null_device()
        endif     
        
        if (present(stderr)) then 
            stderr_file = stderr
        else
            stderr_file = null_device()
        end if
        
        cmd = join(args)//" <"//input_file//" 1>"//stdout_file//" 2>"//stderr_file   
        
    end function assemble_cmd            
    
    !> Returns the file path of the null device for the current operating system.
    !>
    !> Version: Helper function.
    module logical function is_windows()
       is_windows = logical(process_is_windows())
    end function is_windows
    
    !> Reads a whole ASCII file and loads its contents into an allocatable character string..
    !> The function handles error states and optionally deletes the file after reading.
    !> Temporarily uses `linalg_state_type` in lieu of the generalized `state_type`.
    !> 
    !> Version: to be replaced after `getfile` is standardized in `stdlib_io`.
    function getfile(fileName,err,delete) result(file)
      !> Input file name
      character(*), intent(in) :: fileName
      !> [optional] State return flag. On error, if not requested, the code will stop.
      type(linalg_state_type), optional, intent(out) :: err
      !> [optional] Delete file after reading? Default: do not delete
      logical, optional, intent(in) :: delete
      !> Return as an allocatable string
      character(:), allocatable :: file
        
      ! Local variables
      character(*), parameter :: CRLF  = achar(13)//new_line('a')
      type(linalg_state_type) :: err0
      character(len=:), allocatable :: fileString
      character(len=512) :: iomsg
      character :: last_char
      integer :: lun,iostat
      integer(int64) :: errpos,fileSize
      logical :: is_present,want_deleted

      ! Initializations
      file = ""
      
      !> Check if the file should be deleted after reading
      if (present(delete)) then 
         want_deleted = delete
      else
         want_deleted = .false.   
      end if

      !> Check file existing
      inquire(file=fileName, exist=is_present)
      if (.not.is_present) then
         err0 = linalg_state_type('getfile',LINALG_ERROR,'File not present:',fileName)
         call linalg_error_handling(err0,err)
         return
      end if
      
      !> Retrieve file size
      inquire(file=fileName,size=fileSize)
      
      invalid_size: if (fileSize<0) then 

          err0 = linalg_state_type('getfile',LINALG_ERROR,fileName,'has invalid size=',fileSize)
          call linalg_error_handling(err0,err)
          return            
            
      endif invalid_size  
            
      ! Read file
      open(newunit=lun,file=fileName, &
           form='unformatted',action='read',access='stream',status='old', &
           iostat=iostat,iomsg=iomsg)
             
      if (iostat/=0) then 
         err0 = linalg_state_type('getfile',LINALG_ERROR,'Cannot open',fileName,'for read:',iomsg)
         call linalg_error_handling(err0,err)
         return
      end if     
      
      remove_trailing_newline: if (fileSize>0) then 
      
         last_char = CRLF(1:1)
         fileSize  = fileSize+1
      
         do while (scan(last_char,CRLF)>0 .and. fileSize>1)
            fileSize = fileSize-1
            read(lun, pos=fileSize, iostat=iostat, iomsg=iomsg) last_char
            
            ! Read error
            if (iostat/=0) then 
                    
                err0 = linalg_state_type('getfile',LINALG_ERROR,iomsg,'(',fileName,'at byte',fileSize,')')
                call linalg_error_handling(err0,err)
                return

            endif            
            
         end do
      endif remove_trailing_newline      
        
      allocate(character(len=fileSize) :: fileString)
        
      read_data: if (fileSize>0) then 
            
          read(lun, pos=1, iostat=iostat, iomsg=iomsg) fileString
            
          ! Read error
          if (iostat/=0) then 
                
              inquire(unit=lun,pos=errpos)                    
              err0 = linalg_state_type('getfile',LINALG_ERROR,iomsg,'(',fileName,'at byte',errpos,')')
              call linalg_error_handling(err0,err)
              return

          endif
            
      end if read_data
                   
      if (want_deleted) then 
         close(lun,iostat=iostat,status='delete')
         if (iostat/=0) err0 = linalg_state_type('getfile',LINALG_ERROR,'Cannot delete',fileName,'after reading')
      else
         close(lun,iostat=iostat)
         if (iostat/=0) err0 = linalg_state_type('getfile',LINALG_ERROR,'Cannot close',fileName,'after reading')
      endif 
      
      ! Process output
      call move_alloc(from=fileString,to=file)
      call linalg_error_handling(err0,err)

    end function getfile

    !> Return process ID
    module function process_get_ID(process) result(ID)
        class(process_type), intent(in) :: process
        !> Return a process ID
        integer(process_ID) :: ID        
        ID = process%id
    end function process_get_ID

end submodule stdlib_system_subprocess
