program example_global_logger
  use stdlib_logger, global => global_logger
  implicit none

  integer :: unit, stat

  call global%add_log_file('error_log.txt', unit, &
                           position='asis', stat=stat)
  if (stat /= success) then
    error stop 'Unable to open "error_log.txt".'
  end if

end program example_global_logger
