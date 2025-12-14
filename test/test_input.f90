module test_input
  use testdrive, only : new_unittest, unittest_type, error_type, check
  use stdlib_io, only : input
  implicit none
  private
  public :: collect

contains

  subroutine collect(tests)
    type(unittest_type), allocatable, intent(out) :: tests(:)

    tests = [ &
      new_unittest("input preserves whitespace", test_input_whitespace), &
      new_unittest("input with prompt",           test_input_prompt), &
      new_unittest("input with iostat",           test_input_iostat), &
      new_unittest("input with iomsg",            test_input_iomsg), &
      new_unittest("input without optional args", test_input_no_args) &
    ]
  end subroutine collect


  subroutine test_input_whitespace(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: s

    call feed_stdin("  abc  ")
    s = input()
    call assert_equal(error, s, "  abc  ")
  end subroutine test_input_whitespace


  subroutine test_input_prompt(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: s

    call write_test_input("abc")
    s = input("Enter value: ")
    call assert_equal(error, s, "abc")
  end subroutine test_input_prompt


  subroutine test_input_iostat(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: s
    integer :: ios

    call write_test_input("abc")
    s = input(iostat=ios)
    call assert_equal(error, ios, 0)
    call assert_equal(error, s, "abc")
  end subroutine test_input_iostat


  subroutine test_input_iomsg(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: s
    character(len=:), allocatable :: msg

    call write_test_input("abc")
    s = input(iomsg=msg)
    call assert_equal(error, s, "abc")
  end subroutine test_input_iomsg


  subroutine test_input_no_args(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: s

    call write_test_input("abc")
    s = input()
    call assert_equal(error, s, "abc")
  end subroutine test_input_no_args

end module test_input


program run_test_input
  use testdrive, only : run_testsuite
  use test_input, only : collect
  implicit none

  call run_testsuite(collect)
end program run_test_input
