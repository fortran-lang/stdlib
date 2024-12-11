module test_getline
    use stdlib_io, only : getline, getfile
    use stdlib_error, only: state_type
    use stdlib_string_type, only : string_type, len, len_trim
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_getline

contains

    !> Collect all exported unit tests
    subroutine collect_getline(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("read-char", test_read_char), &
            new_unittest("read-string", test_read_string), &
            new_unittest("pad-no", test_pad_no), &
            new_unittest("iostat-end", test_iostat_end), &
            new_unittest("closed-unit", test_closed_unit, should_fail=.true.), &
            new_unittest("no-unit", test_no_unit, should_fail=.true.), &
            new_unittest("getfile-no", test_getfile_missing), &
            new_unittest("getfile-empty", test_getfile_empty), &
            new_unittest("getfile-non-empty", test_getfile_non_empty) &
            ]
    end subroutine collect_getline

    subroutine test_read_char(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, i, stat
        character(len=:), allocatable :: line

        open(newunit=io, status="scratch")
        write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
        rewind(io)

        do i = 1, 3
          call getline(io, line, stat)
          call check(error, stat)
          if (allocated(error)) exit
          call check(error, len(line), 3*10**i)
          if (allocated(error)) exit
        end do
        close(io)
    end subroutine test_read_char

    subroutine test_read_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, i, stat
        type(string_type) :: line

        open(newunit=io, status="scratch")
        write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
        rewind(io)

        do i = 1, 3
          call getline(io, line, stat)
          call check(error, stat)
          if (allocated(error)) exit
          call check(error, len(line), 3*10**i)
          if (allocated(error)) exit
        end do
        close(io)
    end subroutine test_read_string

    subroutine test_pad_no(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, i, stat
        character(len=:), allocatable :: line

        open(newunit=io, status="scratch", pad="no")
        write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
        rewind(io)

        do i = 1, 3
          call getline(io, line, stat)
          call check(error, stat)
          if (allocated(error)) exit
          call check(error, len(line), 3*10**i)
          if (allocated(error)) exit
        end do
        close(io)
    end subroutine test_pad_no

    subroutine test_iostat_end(error)
        use, intrinsic :: iso_fortran_env, only : iostat_end
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, i, stat
        character(len=:), allocatable :: line

        open(newunit=io, status="scratch")
        write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
        rewind(io)

        do i = 1, 3
          call getline(io, line, stat)
          call check(error, stat)
          if (allocated(error)) exit
          call check(error, len(line), 3*10**i)
          if (allocated(error)) exit
        end do
        if (.not.allocated(error)) then
          call getline(io, line, stat)
          call check(error, stat, iostat_end)
        end if
        close(io)
    end subroutine test_iostat_end

    subroutine test_closed_unit(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(len=:), allocatable :: line, msg

        open(newunit=io, status="scratch")
        close(io)

        call getline(io, line, stat, msg)
        call check(error, stat, msg)
    end subroutine test_closed_unit

    subroutine test_no_unit(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(len=:), allocatable :: line, msg

        io = -1
        call getline(io, line, stat, msg)
        call check(error, stat, msg)
    end subroutine test_no_unit

    subroutine test_getfile_missing(error)
        !> Test for a missing file.
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: fileContents
        type(state_type) :: err

        fileContents = getfile("nonexistent_file.txt", err)
        
        ! Check that an error was returned
        call check(error, err%error(), "Error not returned on a missing file")
        if (allocated(error)) return
        
    end subroutine test_getfile_missing

    subroutine test_getfile_empty(error)
        !> Test for an empty file.
        type(error_type), allocatable, intent(out) :: error

        integer :: ios
        character(len=:), allocatable :: filename
        type(string_type) :: fileContents
        type(state_type) :: err
        
        ! Get a temporary file name
        filename = "test_getfile_empty.txt"

        ! Create an empty file        
        open(newunit=ios, file=filename, action="write", form="formatted", access="sequential")        
        close(ios)

        ! Read and delete it
        fileContents = getfile(filename, err, delete=.true.)

        call check(error, err%ok(), "Should not return error reading an empty file")
        if (allocated(error)) return
        
        call check(error, len_trim(fileContents) == 0, "String from empty file should be empty")
        if (allocated(error)) return
        
    end subroutine test_getfile_empty

    subroutine test_getfile_non_empty(error)
        !> Test for a non-empty file.
        type(error_type), allocatable, intent(out) :: error

        integer :: ios
        character(len=:), allocatable :: filename
        type(string_type) :: fileContents
        type(state_type) :: err
        
        ! Get a temporary file name
        filename = "test_getfile_size5.txt"

        ! Create a fixed-size file
        open(newunit=ios, file=filename, action="write", form="unformatted", access="stream")        
        write(ios) "12345"
        close(ios)

        ! Read and delete it
        fileContents = getfile(filename, err, delete=.true.)

        call check(error, err%ok(), "Should not return error reading a non-empty file")
        if (allocated(error)) return
        
        call check(error, len_trim(fileContents) == 5, "Wrong string size returned")
        if (allocated(error)) return

    end subroutine test_getfile_non_empty


end module test_getline


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_getline, only : collect_getline
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("getline", collect_getline) &
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
