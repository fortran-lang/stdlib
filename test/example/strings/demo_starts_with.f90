program demo_starts_with
    use stdlib_strings, only: starts_with
    implicit none
    print'(a)', starts_with("pattern", "pat")  ! T
    print'(a)', starts_with("pattern", "ern")  ! F
end program demo_starts_with
