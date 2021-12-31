program test_64_bit_hash_performance
!! Program to compare the relative performance of different 64 bit hash
!! functions

    use stdlib_kinds, only: &
        dp,           &
        int8,         &
        int32,        &
        int64

    use stdlib_hash_64bit

    implicit none

    integer, parameter :: &
        block_size(8) = [ 1, 2, 4, 8, 16, 64, 256, 1024 ]
    integer(int32), parameter :: huge32 = huge(0_int32)
    real(dp), parameter :: hugep1 = real(huge32, dp) + 1.0_dp
    integer, parameter :: rand_power = 16
    integer, parameter :: rand_size = 2**rand_power
    integer, parameter :: test_size = rand_size * 4
    integer, parameter :: repeat = 4
    integer :: index, k
    integer :: lun
    real(dp) :: rand(2)
    integer(int32) :: rand_object(rand_size)
    integer(int8) :: test_object(test_size)


    open( newunit=lun, file="64_bit_hash_performance_log.txt", &
          access="sequential", action="write", form="formatted", &
          position="rewind" )

    do index=1, rand_size
        call random_number(rand)
        if (rand(1) < 0.5_dp) then
            rand_object(index) = ceiling(-rand(2)*hugep1, int32) - 1
        else
            rand_object(index) = floor(rand(2)*hugep1, int32)
        end if
    end do

    test_object(:) = transfer( rand_object, 0_int8, test_size )

    write(lun, '("| Algorithm  | Key Size  | Key #      | Time (s) |")')
    write(lun, '("|            | Bytes     |            |          |")')
    write(lun, '("|------------|-----------|------------|----------|")')

    call test_fnv_1()

    call test_fnv_1a()

    call test_pengy()

    call test_spooky()

contains

    subroutine test_fnv_1()
        integer :: index2
        integer(int64) :: hash
        real :: t1, t2, tdiff
        integer(int64) :: summary(repeat)

        do k=1, size(block_size)
            call cpu_time(t1)
            do index=1, repeat
                do index2=1, test_size, block_size(k)
                    hash = fnv_1_hash( test_object( index2: &
                                                    index2+block_size(k)-1 ) )
                    if (index2 == index) summary(index) = hash
                end do
            end do
            call cpu_time(t2)
            tdiff = t2-t1
            write(lun, '("|", a10, 2x, "|", i8, 3x, "|", 1x, i10, 1x, ' // &
                '"|", f9.5, 1x, "|")') 'FNV-1', &
                block_size(k), repeat*(test_size/block_size(k)), tdiff
        end do

    end subroutine test_fnv_1

    subroutine test_fnv_1a()
        integer :: index2
        integer(int64) :: hash
        real :: t1, t2, tdiff
        integer(int64) :: summary(repeat)

        do k=1, size(block_size)
            call cpu_time(t1)
            do index=1, repeat
                do index2=1, test_size, block_size(k)
                    hash = fnv_1a_hash( test_object( index2: &
                                                     index2+block_size(k)-1 ) )
                    if (index2 == index) summary(index) = hash
                end do
            end do
            call cpu_time(t2)
            tdiff = t2-t1
            write(lun, '("|", a10, 2x, "|", i8, 3x, "|", 1x, i10, 1x, ' // &
                '"|", f9.5, 1x, "|")') 'FNV-1a', &
                block_size(k), repeat*(test_size/block_size(k)), tdiff
        end do

    end subroutine test_fnv_1a

    subroutine test_spooky()
        integer :: index2
        integer(int64) :: hash(2)
        integer(int64) :: seed(2) = [ 0_int64, 0_int64 ]
        real :: t1, t2, tdiff
        integer(int64) :: summary(repeat)

        call new_spooky_hash_seed( seed )
        do k=1, size(block_size)
            call cpu_time(t1)
            do index=1, repeat
                do index2=1, test_size, block_size(k)
                    hash = spooky_hash( test_object( index2: &
                                                     index2+block_size(k)-1 ), &
                                                     seed )
                    if (index2 == index) summary(index) = hash(1)
                end do
            end do
            call cpu_time(t2)
            tdiff = t2-t1
            write(lun, '("|", a10, 2x, "|", i8, 3x, "|", 1x, i10, 1x, ' // &
                '"|", f9.5, 1x, "|")') 'Spooky', &
                block_size(k), repeat*(test_size/block_size(k)), tdiff
        end do

    end subroutine test_spooky

    subroutine test_pengy()
        integer :: index2
        integer(int64) :: hash
        integer(int32) :: seed = int( z'DEADBEEF', int32 )
        real :: t1, t2, tdiff
        integer(int64) :: summary(repeat)

        call new_pengy_hash_seed( seed )
        do k=1, size(block_size)
            call cpu_time(t1)
            do index=1, repeat
                do index2=1, test_size, block_size(k)
                    hash = pengy_hash( test_object( index2: &
                                       index2+block_size(k)-1 ), &
                                       seed )
                    if (index2 == index) summary(index) = hash
                end do
            end do
            call cpu_time(t2)
            tdiff = t2-t1
            write(lun, '("|", a10, 2x, "|", i8, 3x, "|", 1x, i10, 1x, ' // &
                '"|", f9.5, 1x, "|")') 'Pengy', &
                block_size(k), repeat*(test_size/block_size(k)), tdiff
        end do

    end subroutine test_pengy

end program test_64_bit_hash_performance
