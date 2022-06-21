program demo_configure
    use stdlib_logger, only: global => global_logger

    call global%configure(indent=.false., max_width=72)

end program demo_configure
