program generate_key_array

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64

    integer        :: lun
    integer(int8)  :: key_array(2048)
    integer(int32) :: dummy(512)
    real(real64)   :: rand(512)

! Create key array
    call random_number( rand )
    do i=1, 512
        dummy(i) = floor( rand(i) * 2_int64**32 - 2_int64**31, kind=int32 )
    end do
    key_array = transfer( dummy, 0_int8, 2048 )

    open(newunit=lun, file="key_array.bin", form="unformatted", &
        access="stream", status="new", action="write")
    write(lun) key_array
    close(lun)

end program generate_key_array
