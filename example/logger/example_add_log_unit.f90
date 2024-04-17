program example_add_log_unit
  use stdlib_logger, only: global_logger, read_only_error
  implicit none

  character(256) :: iomsg
  integer :: iostat, unit, stat

  open (newunit=unit, file='error_log.txt', &
        form='formatted', status='replace', &
        position='rewind', &
        action='write', iostat=iostat, iomsg=iomsg)

  call global_logger%add_log_unit(unit, stat)

  select case (stat)
  case (read_only_error)
    error stop 'Unable to write to "error_log.txt".'
  end select

end program example_add_log_unit
