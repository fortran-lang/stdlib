program example_log_io_error
  use stdlib_logger, global => global_logger

  character(*), parameter :: filename = 'dummy.txt'
  integer                 :: iostat, lun
  character(128)          :: iomsg
  character(*), parameter :: message = &
                             'Failure in opening "dummy.txt".'

  open (newunit=lun, file=filename, form='formatted', &
        status='old', iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    call global%log_io_error(message, &
                             procedure='EXAMPLE', &
                             iostat=iostat, &
                             iomsg=iomsg)
    error stop 'Error on opening a file'
  end if

end program example_log_io_error
