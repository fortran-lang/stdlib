module test_subprocess
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: process_type, run, runasync, is_running, wait, update, elapsed, is_windows, kill

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_run_synchronous', test_run_synchronous), &
            new_unittest('test_run_asynchronous', test_run_asynchronous), &
            new_unittest('test_process_kill', test_process_kill), &
            new_unittest('test_process_state', test_process_state), &
            new_unittest('test_input_redirection', test_input_redirection) &
        ]
    end subroutine collect_suite

    !> Test running a synchronous process
    subroutine test_run_synchronous(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        character(len=*), parameter :: command = "echo Hello"

        process = run(command, want_stdout=.true.)
        call check(error, process%completed)
        if (allocated(error)) return
        
        call check(error, trim(process%stdout) == "Hello", "stdout=<"//trim(process%stdout)//">, expected <Hello>")
    end subroutine test_run_synchronous

    !> Test running an asynchronous process
    subroutine test_run_asynchronous(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        logical :: running

        ! The closest possible to a cross-platform command that waits
        if (is_windows()) then 
            process = runasync("ping -n 2 127.0.0.1")
        else
            process = runasync("ping -c 2 127.0.0.1")
        endif
        ! Should not be immediately completed
        call check(error, .not. process%completed, "ping process should not complete immediately")
        if (allocated(error)) return

        running = is_running(process)
        call check(error, running, "ping process should still be running immediately after started")
        if (allocated(error)) return

        call wait(process)
        call check(error, process%completed, "process should be complete after `call wait`")
        if (allocated(error)) return

        call check(error, elapsed(process)>1.0e-4, "There should be a non-zero elapsed time")
        
    end subroutine test_run_asynchronous

    !> Test killing an asynchronous process
    subroutine test_process_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        logical :: running, success

        ! Start a long-running process asynchronously
        if (is_windows()) then
            process = runasync("ping -n 10 127.0.0.1")
        else
            process = runasync("ping -c 10 127.0.0.1")
        endif

        ! Ensure the process starts running
        call check(error, .not. process%completed, "Process should not be completed immediately after starting")
        if (allocated(error)) return

        running = is_running(process)
        call check(error, running, "Process should be running immediately after starting")
        if (allocated(error)) return

        ! Kill the process
        call kill(process, success)
        call check(error, success, "Failed to kill the process")
        if (allocated(error)) return

        ! Verify the process is no longer running
        call check(error, .not. is_running(process), "Process should not be running after being killed")
        if (allocated(error)) return

        ! Ensure process state updates correctly after killing
        call check(error, process%completed, "Process should be marked as completed after being killed")
    end subroutine test_process_kill

    !> Test updating and checking process state
    subroutine test_process_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        character(len=*), parameter :: command = "echo Testing"

        process = run(command, want_stdout=.true., want_stderr=.true.)

        call update(process)
        call check(error, process%completed)
        if (allocated(error)) return

        call check(error, process%exit_code == 0, "Check zero exit code")
        if (allocated(error)) return
        
        call check(error, len_trim(process%stderr) == 0, "Check no stderr output")
        if (allocated(error)) return

        call check(error, trim(process%stdout) == "Testing", "stdout=<"//trim(process%stdout)//">, expected <Testing>")
        if (allocated(error)) return
    end subroutine test_process_state

    !> Test input redirection
    subroutine test_input_redirection(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        character(len=*), parameter :: input_string = "Hello Stdin"
        
        if (is_windows()) then 
             ! findstr "^" echoes input lines. 
             ! Note: We need complex quoting because of how arguments are parsed.
             ! Actually, sticking to something simpler if possible. 
             ! "more" implies paging which might hang. "sort" is usually safe.
             process = run("sort", stdin=input_string, want_stdout=.true.)
        else
             process = run("cat", stdin=input_string, want_stdout=.true.)
        endif

        call check(error, process%completed, "Process did not complete")
        if (allocated(error)) return

        call check(error, process%exit_code == 0, "Process failed with non-zero exit code")
        if (allocated(error)) return

        ! Check if output matches input (sort of "Hello Stdin" is "Hello Stdin")
        call check(error, index(process%stdout, input_string) > 0, &
                   "Output <"//trim(process%stdout)//"> should contain <"//input_string//">")
        
    end subroutine test_input_redirection

end module test_subprocess

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_subprocess, only : collect_suite

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("subprocess", collect_suite) &
    ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
