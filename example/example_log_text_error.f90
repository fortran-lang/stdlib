program example_log_text_error
  use stdlib_logger
  implicit none

  character(*), parameter :: filename = 'dummy.txt'
  integer                 :: col_no, line_no, lun, status
  character(128)          :: line
  character(*), parameter :: message = 'Bad text found.'

  open (newunit=lun, file=filename, status='old', &
        form='formatted')
  line_no = 0
  do
    read (lun, fmt='(a)', end=900) line
    line_no = line_no + 1
    call check_line(line, status, col_no)
    if (status /= 0) then
      call global_logger%log_text_error(line, &
                                        col_no, message, filename, line_no)
      error stop 'Error in reading '//filename
    end if
  end do
900 continue

contains

  subroutine check_line(line, status, col_no)
    character(*), intent(in)    :: line
    integer, intent(inout) :: status
    integer, intent(inout) :: col_no
    ! scan the line for forbidden characters
    col_no = scan(line, ".$/")
    ! col_no > 0 means there is a forbidden character
    status = col_no
  end subroutine

end program example_log_text_error
