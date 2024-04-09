program example_configure
  use stdlib_logger, only: global => global_logger
  implicit none

  call global%configure(indent=.false., max_width=72)

end program example_configure
