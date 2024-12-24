module test_subprocess
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: process_type, run, is_running, wait, update

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_run_synchronous', test_run_synchronous), &
            new_unittest('test_run_asynchronous', test_run_asynchronous), &
            new_unittest('test_process_state', test_process_state) &
        ]
    end subroutine collect_suite

    !> Test running a synchronous process
    subroutine test_run_synchronous(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        character(len=*), parameter :: command = "echo Hello"

        process = run(command, wait=.true., want_stdout=.true.)
        call check(error, process%completed)
        if (allocated(error)) return

        call check(error, trim(process%stdout) == "Hello")
    end subroutine test_run_synchronous

    !> Test running an asynchronous process
    subroutine test_run_asynchronous(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        logical :: running
        character(len=*), parameter :: command = "sleep 1"

        process = run(command, wait=.false.)
        call check(error, .not. process%completed)
        if (allocated(error)) return

        running = is_running(process)
        call check(error, running)
        if (allocated(error)) return

        call wait(process)
        call check(error, process%completed)
    end subroutine test_run_asynchronous

    !> Test updating and checking process state
    subroutine test_process_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(process_type) :: process
        character(len=*), parameter :: command = "echo Testing"

        process = run(command, wait=.true., want_stdout=.true., want_stderr=.true.)

        call update(process)
        call check(error, process%completed)
        if (allocated(error)) return

        call check(error, process%exit_code == 0)
        if (allocated(error)) return

        call check(error, trim(process%stdout) == "Testing")
        if (allocated(error)) return
    end subroutine test_process_state

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
