module stdlib_regex
  use stdlib_kinds, only: dp, sp
  implicit none
  private

  public :: regex_type
  public :: regcomp

  type :: regex_type
    private
    character(len=:), allocatable :: pattern
  end type regex_type

contains

  subroutine regcomp(re, pattern, flags, status)
    type(regex_type), intent(out) :: re
    character(len=*), intent(in) :: pattern
    integer, intent(in), optional :: flags
    integer, intent(out), optional :: status

    re%pattern = pattern
    if (present(status)) status = 0
  end subroutine regcomp

end module stdlib_regex
