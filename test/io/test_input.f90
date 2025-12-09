module test_input
    use, intrinsic :: iso_fortran_env, only : input_unit, output_unit
    use stdlib_io, only : input, get_line
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_input

contains

    !> Collect all exported unit tests
    subroutine collect_input(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("check-input-compilation", test_input_compilation), &
            new_unittest("check-input-functional", test_input_functional), &
            new_unittest("check-input-prompt-capture", test_input_prompt_capture), &
            new_unittest("check-input-error-handling", test_input_error_handling) &
            ]
    end subroutine collect_input

    subroutine test_input_compilation(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        ! Simple check that we can reference the user stub and the interface exists.
        ! We avoid actually calling input() here to prevent blocking in CI.
        call check(error, .true.)
    end subroutine test_input_compilation

    !> Functional test: read from a pre-populated file and verify trailing whitespace preserved.
    subroutine test_input_functional(error)
       type(error_type), allocatable, intent(out) :: error
       integer :: unit
       character(len=:), allocatable :: s
       integer :: iostat

       ! Create a small temporary file that contains a line with trailing whitespace.
       open(newunit=unit, file="test_input_temp1.txt", status="replace", action="write", iostat=iostat)
       call check(error, iostat == 0, "failed to create temp file for input functional test")
       write(unit, '(a)') 'hello  '   ! two trailing spaces
       close(unit)

       ! Open temp file on an auxiliary unit and call get_line to simulate input() internal behavior.
       open(newunit=unit, file="test_input_temp1.txt", status="old", action="read", iostat=iostat)
       call check(error, iostat == 0, "failed to open temp file for reading")
       
       call get_line(unit, s, iostat)
       call check(error, iostat == 0, "iostat should be 0 for normal read")
       ! trailing spaces preserved: len > len_trim
       call check(error, len(s) == 7, "length should be 7 (hello + 2 spaces)")
       call check(error, s == 'hello  ', "content should match including trailing spaces")

       ! cleanup
       close(unit, status="delete")
    end subroutine test_input_functional

    !> Capture prompt output by redirecting output_unit to a file and redirecting input_unit to
    !! a prepared file. Then call input(...) which prints the prompt to output_unit and reads
    !! from input_unit. Confirm the prompt was written (without newline) and the returned string.
    subroutine test_input_prompt_capture(error)
       type(error_type), allocatable, intent(out) :: error
       integer :: in_unit, iostat
       character(len=:), allocatable :: s, prompt_content
       integer :: tmp_unit, ios
       
       ! Prepare a temp file that will act as stdin for input()
       open(newunit=in_unit, file="test_input_temp2.txt", status="replace", action="write", iostat=iostat)
       call check(error, iostat == 0, "failed to create temp input file")
       write(in_unit, '(a)') 'world'   ! no trailing spaces
       close(in_unit)

       ! Re-open the file on the special standard input unit number
       open(unit=input_unit, file="test_input_temp2.txt", status="old", action="read", iostat=iostat)
       call check(error, iostat == 0, "failed to open test_input_temp2.txt on input_unit")

       ! Redirect standard output by opening the special output_unit to a capture file.
       open(unit=output_unit, file="prompt_capture.txt", status="replace", action="write", iostat=iostat)
       call check(error, iostat == 0, "failed to redirect output_unit to prompt_capture.txt")

       ! Call input which should:
       !  - write the prompt to output_unit (no newline)
       !  - read the line from input_unit
       s = input("PROMPT: ", iostat)
       call check(error, iostat == 0, "input() iostat must be 0 when reading from prepared file")
       call check(error, trim(s) == 'world', "input must read 'world' from prepared stdin file")

       ! Close redirected units to flush output and restore state
       close(unit=output_unit, iostat=ios)
       call check(error, ios == 0, "failed to close redirected output_unit")
       close(unit=input_unit, iostat=ios)
       call check(error, ios == 0, "failed to close redirected input_unit")

       ! Read the prompt capture file and verify prompt was printed.
       open(newunit=tmp_unit, file="prompt_capture.txt", status="old", action="read", iostat=iostat)
       call check(error, iostat == 0, "failed to open prompt_capture.txt for verification")
       call get_line(tmp_unit, prompt_content, iostat)
       call check(error, iostat == 0, "failed to read prompt_capture.txt")
       close(tmp_unit, status="delete")

       ! The prompt should be present. Because input printed prompt with advance='no', the file
       ! should contain the prompt text (no trailing newline before prompt). Check prefix.
       call check(error, index(prompt_content, "PROMPT:") == 1, "prompt must appear at start of captured output")
    end subroutine test_input_prompt_capture

    !> Error handling test: ensure input returns non-zero iostat when input_unit is not connected.
    subroutine test_input_error_handling(error)
       type(error_type), allocatable, intent(out) :: error
       integer :: ios
       character(len=:), allocatable :: s
       integer :: iostat

       ! Ensure input_unit is not connected by trying to close it (ignore errors)
       close(unit=input_unit, iostat=ios)

       ! Now call input(...) with iostat and expect non-zero (error/EOF)
       s = input("ShouldFail: ", iostat)
       call check(error, iostat /= 0, "input iostat should be non-zero if input_unit is not connected")

       ! If the unit was mistakenly connected, try to forcibly open then close to simulate disconnected state.
       ! (No-op if already disconnected)
       if (iostat == 0) then
           ! attempt to close properly to simulate failure on subsequent tries
           close(unit=input_unit, iostat=ios)
           s = input("ShouldFail2: ", iostat)
           call check(error, iostat /= 0, "input iostat should be non-zero after closing input_unit")
       end if
    end subroutine test_input_error_handling

end module test_input

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_input, only : collect_input
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("input", collect_input) &
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
