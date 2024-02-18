module test_zip
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp
    use stdlib_io_zip, only: t_unzipped_bundle, unzip
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_np

contains

    subroutine collect_np(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest('unexistent-file', test_unexistent_file, should_fail=.true.), &
                    new_unittest('empty-zip', test_empty_zip), &
                    new_unittest('empty-array', test_empty_array), &
                    new_unittest('single-file', test_single_file), &
                    new_unittest('two-files', test_two_files) &
                    ]
    end

    subroutine test_unexistent_file(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'unexistent-file.zip'
        type(t_unzipped_bundle) :: bundle
        integer :: stat
        character(len=:), allocatable :: msg

        call unzip(filename, bundle, stat, msg)
        call check(error, stat, msg)
    end

    subroutine test_empty_zip(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'test_empty_zip.zip'
        type(t_unzipped_bundle) :: bundle
        integer :: io, stat
        character(len=:), allocatable :: msg

        character(*), parameter:: binary_data = 'PK'//char(5)//char(6)//repeat(char(0), 18)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, bundle, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(bundle%files) == 0, 'Files should be empty')
    end

    subroutine test_empty_array(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'test_empty_array.zip'
        type(t_unzipped_bundle) :: bundle
        integer :: io, stat
        character(len=:), allocatable :: msg

        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//'6H[s'// &
            & repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))//char(0)//'arr_0.npy'//char(1)//char(0)// &
            & char(int(z'10'))//char(0)//char(int(z'80'))//repeat(char(0), 7)//char(int(z'80'))//repeat(char(0), 7)// &
            & char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<f8', 'fortran_order': False, 'shape': (0,), }"//repeat(' ', 60)//char(int(z'0a'))//'PK'// &
            & char(1)//char(2)//'-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//'6H[s'//char(int(z'80'))// &
            & repeat(char(0), 3)//char(int(z'80'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)// &
            & char(int(z'80'))//char(1)//repeat(char(0), 4)//'arr_0.npyPK'//char(5)//char(6)//repeat(char(0), 4)// &
            & char(1)//char(0)//char(1)//char(0)//'7'//repeat(char(0), 3)//char(int(z'bb'))//repeat(char(0), 5)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, bundle, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(bundle%files) == 1, 'Number of files should be 1')
        call check(error, bundle%files(1)%name == 'arr_0.npy', "Name of file is '"//bundle%files(1)%name//"', not 'arr_0.npy'")
    end

    subroutine test_single_file(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'test_single_file.zip'
        type(t_unzipped_bundle) :: bundle
        integer :: io, stat
        character(len=:), allocatable :: msg

        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//'&M'// &
            & char(int(z'b0'))//char(int(z'd8'))//repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))// &
            & char(0)//'arr_0.npy'//char(1)//char(0)//char(int(z'10'))//char(0)//char(int(z'98'))//repeat(char(0), 7)// &
            & char(int(z'98'))//repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<i8', 'fortran_order': False, 'shape': (3,), }"//repeat(' ', 60)//char(int(z'0a'))// &
            & char(2)//repeat(char(0), 7)//char(4)//repeat(char(0), 7)//char(8)//repeat(char(0), 7)//'PK'//char(1)//char(2)// &
            & '-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//'&M'//char(int(z'b0'))//char(int(z'd8'))//char(int(z'98'))// &
            & repeat(char(0), 3)//char(int(z'98'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)//char(int(z'80'))//char(1)// &
            & repeat(char(0), 4)//'arr_0.npyPK'//char(5)//char(6)//repeat(char(0), 4)//char(1)//char(0)//char(1)// &
            & char(0)//'7'//repeat(char(0), 3)//char(int(z'd3'))//repeat(char(0), 5)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, bundle, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(bundle%files) == 1, 'Number of files should be 1')
        call check(error, bundle%files(1)%name == 'arr_0.npy', "Name of file is '"//bundle%files(1)%name//"', not 'arr_0.npy'")
    end

    subroutine test_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'test_two_files.zip'
        type(t_unzipped_bundle) :: bundle
        integer :: io, stat
        character(len=:), allocatable :: msg

        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'a0'))// &
            & 'DK['//repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))//char(0)//'arr_0.npy'//char(1)// &
            & char(0)//char(int(z'10'))//char(0)//char(int(z'a0'))//repeat(char(0), 7)//char(int(z'a0'))// &
            & repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<i8', 'fortran_order': False, 'shape': (2, 2), }"//repeat(' ', 58)//char(int(z'0a'))//char(1)// &
            & repeat(char(0), 7)//char(2)//repeat(char(0), 7)//char(3)//repeat(char(0), 7)//char(4)//repeat(char(0), 7)//'PK'// &
            & char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'f0'))//'zM?'//repeat(char(int(z'ff')), 8)// &
            & char(9)//char(0)//char(int(z'14'))//char(0)//'arr_1.npy'//char(1)//char(0)//char(int(z'10'))//char(0)// &
            & char(int(z'90'))//repeat(char(0), 7)//char(int(z'90'))//repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)// &
            & char(0)//'v'//char(0)//"{'descr': '<f8', 'fortran_order': False, 'shape': (2,), }"//repeat(' ', 60)// &
            & char(int(z'0a'))//'333333'//char(int(z'f3'))//'?333333'//char(int(z'0b'))//'@PK'//char(1)//char(2)//'-'// &
            & char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'a0'))//'DK['//char(int(z'a0'))//repeat(char(0), 3)// &
            & char(int(z'a0'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)//char(int(z'80'))//char(1)//repeat(char(0), 4)// &
            & 'arr_0.npyPK'//char(1)//char(2)//'-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'f0'))//'zM?'// &
            & char(int(z'90'))//repeat(char(0), 3)//char(int(z'90'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)// &
            & char(int(z'80'))//char(1)//char(int(z'db'))//repeat(char(0), 3)//'arr_1.npyPK'//char(5)//char(6)// &
            & repeat(char(0), 4)//char(2)//char(0)//char(2)//char(0)//'n'//repeat(char(0), 3)//char(int(z'a6'))//char(1)// &
            & repeat(char(0), 4)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, bundle, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(bundle%files) == 2, 'Number of files should be 2')
        call check(error, bundle%files(1)%name == 'arr_0.npy', "Name of file is '"//bundle%files(1)%name//"', not 'arr_0.npy'")
        call check(error, bundle%files(2)%name == 'arr_1.npy', "Name of file is '"//bundle%files(2)%name//"', not 'arr_1.npy'")
    end

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open (newunit=io, file=filename)
        close (io, status='delete')
    end
end

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_zip, only: collect_np
    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite('zip', collect_np) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) 'Testing:', testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, 'test(s) failed!'
        error stop
    end if
end program
