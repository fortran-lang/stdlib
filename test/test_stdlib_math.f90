! SPDX-Identifier: MIT


module test_stdlib_math
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_math, only: clip, swap, arg, argd, argpi, arange, is_close, all_close, diff, &
                           arange, deg2rad, rad2deg
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    implicit none

    public :: collect_stdlib_math
    
    real(kind=sp), parameter :: PI_sp = acos(-1.0_sp)
    real(kind=dp), parameter :: PI_dp = acos(-1.0_dp)

contains

    !> Collect all exported unit tests
    subroutine collect_stdlib_math(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("clip-int8", test_clip_int8), &
            new_unittest("clip-int8-bounds", test_clip_int8_bounds), &
            new_unittest("clip-int16", test_clip_int16), &
            new_unittest("clip-int16-bounds", test_clip_int16_bounds), &
            new_unittest("clip-int32", test_clip_int32), &
            new_unittest("clip-int32-bounds", test_clip_int32_bounds), &
            new_unittest("clip-int64", test_clip_int64), &
            new_unittest("clip-int64-bounds", test_clip_int64_bounds), &
            new_unittest("clip-real-single", test_clip_rsp), &
            new_unittest("clip-real-single-bounds", test_clip_rsp_bounds), &
            new_unittest("clip-real-double", test_clip_rdp), &
            new_unittest("clip-real-double-bounds", test_clip_rdp_bounds), &
            new_unittest("clip-real-quad", test_clip_rqp), &
            new_unittest("clip-real-quad-bounds", test_clip_rqp_bounds) &
            
            !> Tests swap
            , new_unittest("swap_int8", test_swap_int8) &
            , new_unittest("swap_int16", test_swap_int16) &
            , new_unittest("swap_int32", test_swap_int32) &
            , new_unittest("swap_int64", test_swap_int64) &
            , new_unittest("swap_sp", test_swap_sp) &
            , new_unittest("swap_dp", test_swap_dp) &
            , new_unittest("swap_csp", test_swap_csp) &
            , new_unittest("swap_cdp", test_swap_cdp) &
            , new_unittest("swap_str", test_swap_str) &
            , new_unittest("swap_stt", test_swap_stt) &

            !> Tests for arg/argd/argpi
            , new_unittest("arg-cmplx-sp", test_arg_sp) &
            , new_unittest("argd-cmplx-sp", test_argd_sp) &
            , new_unittest("argpi-cmplx-sp", test_argpi_sp) &
            , new_unittest("arg-cmplx-dp", test_arg_dp) &
            , new_unittest("argd-cmplx-dp", test_argd_dp) &
            , new_unittest("argpi-cmplx-dp", test_argpi_dp) &
            
            !> Tests for deg2rad/rad2deg
            , new_unittest("deg2rad-real-sp", test_deg2rad_sp) &
            , new_unittest("rad2deg-real-sp", test_rad2deg_sp) &
            , new_unittest("deg2rad-real-dp", test_deg2rad_dp) &
            , new_unittest("rad2deg-real-dp", test_rad2deg_dp) &
            
            !> Tests for `is_close` and `all_close`
            , new_unittest("is_close-real-sp", test_is_close_real_sp) &
            , new_unittest("is_close-cmplx-sp", test_is_close_cmplx_sp) &
            , new_unittest("all_close-real-sp", test_all_close_real_sp) &
            , new_unittest("all_close-cmplx-sp", test_all_close_cmplx_sp) &
            , new_unittest("is_close-real-dp", test_is_close_real_dp) &
            , new_unittest("is_close-cmplx-dp", test_is_close_cmplx_dp) &
            , new_unittest("all_close-real-dp", test_all_close_real_dp) &
            , new_unittest("all_close-cmplx-dp", test_all_close_cmplx_dp) &
            
            !> Tests for `diff`
            , new_unittest("diff-real-sp", test_diff_real_sp) &
            , new_unittest("diff-real-dp", test_diff_real_dp) &
            , new_unittest("diff-int-int8", test_diff_int_int8) &
            , new_unittest("diff-int-int16", test_diff_int_int16) &
            , new_unittest("diff-int-int32", test_diff_int_int32) &
            , new_unittest("diff-int-int64", test_diff_int_int64) &
            
            !> Tests for `arange`
            , new_unittest("arange-real-sp", test_arange_real_sp) &
            , new_unittest("arange-real-dp", test_arange_real_dp) &
            , new_unittest("arange-int-int8", test_arange_int_int8) &
            , new_unittest("arange-int-int16", test_arange_int_int16) &
            , new_unittest("arange-int-int32", test_arange_int_int32) &
            , new_unittest("arange-int-int64", test_arange_int_int64) &
            ]

    end subroutine collect_stdlib_math

    subroutine test_clip_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! clip function
        ! testing format: check(clip(x, xmin, xmax) == correct answer)
        ! valid case: xmin is not greater than xmax
        ! invalid case: xmin is greater than xmax

        ! type: integer(int8), kind: int8
        ! valid test case
        call check(error, clip(2_int8, -2_int8, 5_int8), 2_int8)
        if (allocated(error)) return
        call check(error, clip(127_int8, -127_int8, 0_int8), 0_int8)
        if (allocated(error)) return
    end subroutine test_clip_int8

    subroutine test_clip_int8_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int8, 5_int8, -2_int8), 5_int8)
        if (allocated(error)) return
        call check(error, clip(127_int8, 0_int8, -127_int8), 0_int8)
        if (allocated(error)) return
    end subroutine test_clip_int8_bounds


    subroutine test_clip_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int16), kind: int16
        ! valid test case
        call check(error, clip(2_int16, -2_int16, 5_int16), 2_int16)
        if (allocated(error)) return
        call check(error, clip(32767_int16, -32767_int16, 0_int16), 0_int16)
        if (allocated(error)) return
    end subroutine test_clip_int16

    subroutine test_clip_int16_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int16, 5_int16, -2_int16), 5_int16)
        if (allocated(error)) return
        call check(error, clip(32767_int16, 0_int16, -32767_int16), 0_int16)
        if (allocated(error)) return
    end subroutine test_clip_int16_bounds


    subroutine test_clip_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int32), kind: int32
        ! valid test case
        call check(error, clip(2_int32, -2_int32, 5_int32), 2_int32)
        if (allocated(error)) return
        call check(error, clip(-2147483647_int32, 0_int32, 2147483647_int32), 0_int32)
        if (allocated(error)) return
    end subroutine test_clip_int32

    subroutine test_clip_int32_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int32, 5_int32, -2_int32), 5_int32)
        if (allocated(error)) return
        call check(error, clip(-2147483647_int32, 2147483647_int32, 0_int32), 2147483647_int32)
        if (allocated(error)) return
    end subroutine test_clip_int32_bounds


    subroutine test_clip_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int64), kind: int64
        ! valid test case
        call check(error, clip(2_int64, -2_int64, 5_int64), 2_int64)
        if (allocated(error)) return
        call check(error, clip(-922337203_int64, -10_int64, 25_int64), -10_int64)
        if (allocated(error)) return
    end subroutine test_clip_int64

    subroutine test_clip_int64_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int64, 5_int64, -2_int64), 5_int64)
        if (allocated(error)) return
        call check(error, clip(-922337203_int64, 25_int64, -10_int64), 25_int64)
        if (allocated(error)) return
    end subroutine test_clip_int64_bounds


    subroutine test_clip_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: real(sp), kind: sp
        ! valid test case
        call check(error, clip(3.025_sp, -5.77_sp, 3.025_sp), 3.025_sp)
        if (allocated(error)) return
        call check(error, clip(0.0_sp, -1578.025_sp, -59.68_sp), -59.68_sp)
        if (allocated(error)) return
    end subroutine test_clip_rsp

    subroutine test_clip_rsp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(3.025_sp, 3.025_sp, -5.77_sp), 3.025_sp)
        if (allocated(error)) return
        call check(error, clip(0.0_sp, -59.68_sp, -1578.025_sp), -59.68_sp)
        if (allocated(error)) return
    end subroutine test_clip_rsp_bounds


    subroutine test_clip_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: real(dp), kind: dp
        ! valid test case
        call check(error, clip(3.025_dp, -5.77_dp, 3.025_dp), 3.025_dp)
        if (allocated(error)) return
        call check(error, clip(-7.0_dp, 0.059668_dp, 1.00268_dp), 0.059668_dp)
        if (allocated(error)) return
    end subroutine test_clip_rdp

    subroutine test_clip_rdp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(3.025_dp, 3.025_dp, -5.77_dp), 3.025_dp)
        if (allocated(error)) return
        call check(error, clip(-7.0_dp, 1.00268_dp, 0.059668_dp), 1.00268_dp)
        if (allocated(error)) return
    end subroutine test_clip_rdp_bounds


    subroutine test_clip_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_clip_rqp


    subroutine test_clip_rqp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")

    end subroutine test_clip_rqp_bounds

    subroutine test_swap_int8(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int8) :: x(3), y(3)
        
        x = [integer(int8) :: 1, 2, 3]
        y = [integer(int8) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [integer(int8) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [integer(int8) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [integer(int8) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_int8
    subroutine test_swap_int16(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int16) :: x(3), y(3)
        
        x = [integer(int16) :: 1, 2, 3]
        y = [integer(int16) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [integer(int16) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [integer(int16) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [integer(int16) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_int16
    subroutine test_swap_int32(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: x(3), y(3)
        
        x = [integer(int32) :: 1, 2, 3]
        y = [integer(int32) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [integer(int32) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [integer(int32) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [integer(int32) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_int32
    subroutine test_swap_int64(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: x(3), y(3)
        
        x = [integer(int64) :: 1, 2, 3]
        y = [integer(int64) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [integer(int64) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [integer(int64) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [integer(int64) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_int64
    subroutine test_swap_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: x(3), y(3)
        
        x = [real(sp) :: 1, 2, 3]
        y = [real(sp) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [real(sp) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [real(sp) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [real(sp) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_sp
    subroutine test_swap_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: x(3), y(3)
        
        x = [real(dp) :: 1, 2, 3]
        y = [real(dp) :: 4, 5, 6]

        call swap(x,y)
        
        call check(error, all( x == [real(dp) :: 4, 5, 6] ) )
        if (allocated(error)) return
        call check(error, all( y == [real(dp) :: 1, 2, 3] ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)

        call check(error, all( x == [real(dp) :: 4, 5, 6] ) )
        if (allocated(error)) return
    end subroutine test_swap_dp

    subroutine test_swap_csp(error)
        type(error_type), allocatable, intent(out) :: error
        complex(sp) :: x(3), y(3)
        
        x = cmplx( [1, 2, 3] , [4, 5, 6] )
        y = cmplx( [4, 5, 6] , [1, 2, 3] )

        call swap(x,y)
        
        call check(error, all( x == cmplx( [4, 5, 6] , [1, 2, 3] ) ) )
        if (allocated(error)) return
        call check(error, all( y == cmplx( [1, 2, 3] , [4, 5, 6] ) ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)
        
        call check(error, all( x == cmplx( [4, 5, 6] , [1, 2, 3] ) ) )
        if (allocated(error)) return
    end subroutine test_swap_csp
    subroutine test_swap_cdp(error)
        type(error_type), allocatable, intent(out) :: error
        complex(dp) :: x(3), y(3)
        
        x = cmplx( [1, 2, 3] , [4, 5, 6] )
        y = cmplx( [4, 5, 6] , [1, 2, 3] )

        call swap(x,y)
        
        call check(error, all( x == cmplx( [4, 5, 6] , [1, 2, 3] ) ) )
        if (allocated(error)) return
        call check(error, all( y == cmplx( [1, 2, 3] , [4, 5, 6] ) ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)
        
        call check(error, all( x == cmplx( [4, 5, 6] , [1, 2, 3] ) ) )
        if (allocated(error)) return
    end subroutine test_swap_cdp

    subroutine test_swap_str(error)
        type(error_type), allocatable, intent(out) :: error
        block
            character(5) :: x(2), y(2)
            
            x = ['abcde','fghij'] 
            y = ['fghij','abcde']

            call swap(x,y)
            
            call check(error, all( x == ['fghij','abcde'] ) )
            if (allocated(error)) return
            call check(error, all( y == ['abcde','fghij']  ) )
            if (allocated(error)) return

            ! check self swap
            call swap(x,x)
            
            call check(error, all( x == ['fghij','abcde'] ) )
            if (allocated(error)) return
        end block

        block
            character(4) :: x
            character(6) :: y
            
            x = 'abcd'
            y = 'efghij'
            call swap(x,y)
            
            call check(error, x == 'efgh' )
            if (allocated(error)) return
            call check(error, y(1:6) == 'abcd  ' )
            if (allocated(error)) return

            x = 'abcd'
            y = 'efghij'
            call swap(x,y(1:4))
            
            call check(error, x == 'efgh' )
            if (allocated(error)) return
            call check(error, y == 'abcdij' )
            if (allocated(error)) return
        end block
    end subroutine test_swap_str

    subroutine test_swap_stt(error)
        use stdlib_string_type
        type(error_type), allocatable, intent(out) :: error
        type(string_type) :: x(2), y(2)
        
        x = ['abcde','fghij'] 
        y = ['fghij','abcde']

        call swap(x,y)
        
        call check(error, all( x == ['fghij','abcde'] ) )
        if (allocated(error)) return
        call check(error, all( y == ['abcde','fghij']  ) )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)
        
        call check(error, all( x == ['fghij','abcde'] ) )
        if (allocated(error)) return
    end subroutine test_swap_stt

    subroutine test_swap_bitset_64(error)
        use stdlib_bitsets
        type(error_type), allocatable, intent(out) :: error
        type(bitset_64) :: x, y, u, v
        
        x = [.true.,.false.,.true.,.false.] 
        u = x
        y = [.false.,.true.,.false.,.true.]
        v = y
        call swap(x,y)
        
        call check(error, x == v )
        if (allocated(error)) return
        call check(error, y == u  )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)
        
        call check(error, x == v )
        if (allocated(error)) return
    end subroutine test_swap_bitset_64

    subroutine test_swap_bitset_large(error)
        use stdlib_bitsets
        type(error_type), allocatable, intent(out) :: error
        type(bitset_large) :: x, y, u, v
        
        x = [.true.,.false.,.true.,.false.] 
        u = x
        y = [.false.,.true.,.false.,.true.]
        v = y
        call swap(x,y)
        
        call check(error, x == v )
        if (allocated(error)) return
        call check(error, y == u )
        if (allocated(error)) return

        ! check self swap
        call swap(x,x)
        
        call check(error, x == v )
        if (allocated(error)) return
    end subroutine test_swap_bitset_large

    subroutine test_arg_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp), parameter :: tol = sqrt(epsilon(1.0_sp))
        real(sp), allocatable :: theta(:)
        
        call check(error, abs(arg(2*exp((0.0_sp, 0.5_sp))) - 0.5_sp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(arg((0.0_sp, 0.0_sp)) - 0.0_sp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_sp, 179.0_sp, 3.58_sp)
        call check(error, all(abs(arg(exp(cmplx(0.0_sp, theta/180*PI_sp, sp))) - theta/180*PI_sp) < tol), &
            "test_array")
        
    end subroutine test_arg_sp
    
    subroutine test_argd_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp), parameter :: tol = sqrt(epsilon(1.0_sp))
        real(sp), allocatable :: theta(:)
        
        call check(error, abs(argd((-1.0_sp, 0.0_sp)) - 180.0_sp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(argd((0.0_sp, 0.0_sp)) - 0.0_sp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_sp, 179.0_sp, 3.58_sp)
        call check(error, all(abs(argd(exp(cmplx(0.0_sp, theta/180*PI_sp, sp))) - theta) < tol), &
            "test_array")
        
    end subroutine test_argd_sp
    
    subroutine test_argpi_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp), parameter :: tol = sqrt(epsilon(1.0_sp))
        real(sp), allocatable :: theta(:)
        
        call check(error, abs(argpi((-1.0_sp, 0.0_sp)) - 1.0_sp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(argpi((0.0_sp, 0.0_sp)) - 0.0_sp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_sp, 179.0_sp, 3.58_sp)
        call check(error, all(abs(argpi(exp(cmplx(0.0_sp, theta/180*PI_sp, sp))) - theta/180) < tol), &
            "test_array")
        
    end subroutine test_argpi_sp
    subroutine test_arg_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp), parameter :: tol = sqrt(epsilon(1.0_dp))
        real(dp), allocatable :: theta(:)
        
        call check(error, abs(arg(2*exp((0.0_dp, 0.5_dp))) - 0.5_dp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(arg((0.0_dp, 0.0_dp)) - 0.0_dp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_dp, 179.0_dp, 3.58_dp)
        call check(error, all(abs(arg(exp(cmplx(0.0_dp, theta/180*PI_dp, dp))) - theta/180*PI_dp) < tol), &
            "test_array")
        
    end subroutine test_arg_dp
    
    subroutine test_argd_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp), parameter :: tol = sqrt(epsilon(1.0_dp))
        real(dp), allocatable :: theta(:)
        
        call check(error, abs(argd((-1.0_dp, 0.0_dp)) - 180.0_dp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(argd((0.0_dp, 0.0_dp)) - 0.0_dp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_dp, 179.0_dp, 3.58_dp)
        call check(error, all(abs(argd(exp(cmplx(0.0_dp, theta/180*PI_dp, dp))) - theta) < tol), &
            "test_array")
        
    end subroutine test_argd_dp
    
    subroutine test_argpi_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp), parameter :: tol = sqrt(epsilon(1.0_dp))
        real(dp), allocatable :: theta(:)
        
        call check(error, abs(argpi((-1.0_dp, 0.0_dp)) - 1.0_dp) < tol, &
            "test_nonzero_scalar")
        if (allocated(error)) return
        call check(error, abs(argpi((0.0_dp, 0.0_dp)) - 0.0_dp) < tol, &
            "test_zero_scalar")
        if (allocated(error)) return
        
        theta = arange(-179.0_dp, 179.0_dp, 3.58_dp)
        call check(error, all(abs(argpi(exp(cmplx(0.0_dp, theta/180*PI_dp, dp))) - theta/180) < tol), &
            "test_array")
        
    end subroutine test_argpi_dp
    
    subroutine test_deg2rad_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp), parameter :: tol = sqrt(epsilon(1.0_sp))

        call check(error, PI_sp, deg2rad(180.0_sp), thr=tol)
        if (allocated(error)) return

    end subroutine test_deg2rad_sp
    
    subroutine test_rad2deg_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp), parameter :: tol = sqrt(epsilon(1.0_sp))

        call check(error, 180.0_sp, rad2deg(PI_sp), thr=tol)
        if (allocated(error)) return

    end subroutine test_rad2deg_sp
    subroutine test_deg2rad_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp), parameter :: tol = sqrt(epsilon(1.0_dp))

        call check(error, PI_dp, deg2rad(180.0_dp), thr=tol)
        if (allocated(error)) return

    end subroutine test_deg2rad_dp
    
    subroutine test_rad2deg_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp), parameter :: tol = sqrt(epsilon(1.0_dp))

        call check(error, 180.0_dp, rad2deg(PI_dp), thr=tol)
        if (allocated(error)) return

    end subroutine test_rad2deg_dp

    subroutine test_is_close_real_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: x, NAN
        x = -3; NAN = sqrt(x)
        
        call check(error, is_close(2.5_sp, 2.5_sp), .true.)
        if (allocated(error)) return
        call check(error, is_close(0.0_sp, -0.0_sp), .true.)
        if (allocated(error)) return
        call check(error, is_close(2.5_sp, 1.2_sp), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, NAN), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, 0.0_sp), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, NAN, equal_nan=.true.), .true.)
    
    end subroutine test_is_close_real_sp
    
    subroutine test_is_close_cmplx_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: x, NAN
        x = -3; NAN = sqrt(x)
        
        call check(error, is_close((2.5_sp, 1.5_sp), (2.5_sp, 1.5_sp)), .true.)
        if (allocated(error)) return
        call check(error, is_close((2.5_sp, 1.2_sp), (2.5_sp, 1.5_sp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, sp), cmplx(NAN, NAN, sp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, sp), cmplx(NAN, 0.0_sp, sp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, sp), cmplx(NAN, NAN, sp), equal_nan=.true.), .true.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, 1.2_sp, sp), cmplx(NAN, 1.2_sp, sp), equal_nan=.true.), .true.)
        
    end subroutine test_is_close_cmplx_sp
    
    subroutine test_all_close_real_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: x(2, 2), eps, NAN
        x = 1; eps = -3; NAN = sqrt(eps)
        
        eps = sqrt(epsilon(1.0_sp))
        
        call check(error, all_close(x, x), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + x*eps + 1.0e-6, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x, equal_nan=.true.), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x + NAN), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x + NAN, equal_nan=.true.), .true.)
        
    end subroutine test_all_close_real_sp
    
    subroutine test_all_close_cmplx_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: eps, NAN
        complex(sp) :: x(2, 2)
        x = (1, 1); eps = -3; NAN = sqrt(eps)
        
        eps = sqrt(epsilon(1.0_sp))
        
        call check(error, all_close(x, x), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + x*eps + 1.0e-6, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, sp), x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, sp), x, equal_nan=.true.), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, sp), x + cmplx(NAN, NAN, sp), equal_nan=.true.), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, sp), x + cmplx(NAN, NAN, sp)), .false.)
        
    end subroutine test_all_close_cmplx_sp
    subroutine test_is_close_real_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: x, NAN
        x = -3; NAN = sqrt(x)
        
        call check(error, is_close(2.5_dp, 2.5_dp), .true.)
        if (allocated(error)) return
        call check(error, is_close(0.0_dp, -0.0_dp), .true.)
        if (allocated(error)) return
        call check(error, is_close(2.5_dp, 1.2_dp), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, NAN), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, 0.0_dp), .false.)
        if (allocated(error)) return
        call check(error, is_close(NAN, NAN, equal_nan=.true.), .true.)
    
    end subroutine test_is_close_real_dp
    
    subroutine test_is_close_cmplx_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: x, NAN
        x = -3; NAN = sqrt(x)
        
        call check(error, is_close((2.5_dp, 1.5_dp), (2.5_dp, 1.5_dp)), .true.)
        if (allocated(error)) return
        call check(error, is_close((2.5_dp, 1.2_dp), (2.5_dp, 1.5_dp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, dp), cmplx(NAN, NAN, dp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, dp), cmplx(NAN, 0.0_dp, dp)), .false.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, NAN, dp), cmplx(NAN, NAN, dp), equal_nan=.true.), .true.)
        if (allocated(error)) return
        call check(error, is_close(cmplx(NAN, 1.2_dp, dp), cmplx(NAN, 1.2_dp, dp), equal_nan=.true.), .true.)
        
    end subroutine test_is_close_cmplx_dp
    
    subroutine test_all_close_real_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: x(2, 2), eps, NAN
        x = 1; eps = -3; NAN = sqrt(eps)
        
        eps = sqrt(epsilon(1.0_dp))
        
        call check(error, all_close(x, x), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + x*eps + 1.0e-6, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x, equal_nan=.true.), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x + NAN), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + NAN, x + NAN, equal_nan=.true.), .true.)
        
    end subroutine test_all_close_real_dp
    
    subroutine test_all_close_cmplx_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: eps, NAN
        complex(dp) :: x(2, 2)
        x = (1, 1); eps = -3; NAN = sqrt(eps)
        
        eps = sqrt(epsilon(1.0_dp))
        
        call check(error, all_close(x, x), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + x*eps + 1.0e-6, x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, dp), x), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, dp), x, equal_nan=.true.), .false.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, dp), x + cmplx(NAN, NAN, dp), equal_nan=.true.), .true.)
        if (allocated(error)) return
        call check(error, all_close(x + cmplx(NAN, NAN, dp), x + cmplx(NAN, NAN, dp)), .false.)
        
    end subroutine test_all_close_cmplx_dp
    
    subroutine test_diff_real_sp(error)
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: x(6) = [real(sp) :: 0, 5, 15, 30, 50, 75]
        real(sp) :: A(1, 3) = reshape([real(sp) :: 1, 3, 5], [1, 3])
        real(sp) :: B(2) = [real(sp) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all_close(diff(x), [real(sp) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_real_sp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, n=0), x), &
            "diff(<rank-1>, n=0) in test_diff_real_sp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, n=2), [real(sp) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_real_sp failed")
        if (allocated(error)) return
        
        call check(error, all_close(diff(x, prepend=[real(sp) :: 1]), [real(sp) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[real(sp) :: 1]) in test_diff_real_sp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, append=[real(sp) :: 1]), [real(sp) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[real(sp) :: 1]) in test_diff_real_sp failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all_close(diff(reshape(A, [3,1]), n=1, dim=1), reshape([real(sp) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_real_sp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(A, n=1, dim=2), reshape([real(sp) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_real_sp failed")
        if (allocated(error)) return
        
        call check(error, all_close(diff(A, n=1, dim=2, prepend=reshape([real(sp) :: 1], [1, 1]), &
            append=reshape([real(sp) :: 2], [1, 1])), reshape([real(sp) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([real(sp) :: 1], [1, 1]), &
            &append=reshape([real(sp) :: 2], [1, 1])) in test_diff_real_sp failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_real_sp failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_real_sp failed")
        
    end subroutine test_diff_real_sp
    subroutine test_diff_real_dp(error)
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: x(6) = [real(dp) :: 0, 5, 15, 30, 50, 75]
        real(dp) :: A(1, 3) = reshape([real(dp) :: 1, 3, 5], [1, 3])
        real(dp) :: B(2) = [real(dp) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all_close(diff(x), [real(dp) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_real_dp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, n=0), x), &
            "diff(<rank-1>, n=0) in test_diff_real_dp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, n=2), [real(dp) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_real_dp failed")
        if (allocated(error)) return
        
        call check(error, all_close(diff(x, prepend=[real(dp) :: 1]), [real(dp) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[real(dp) :: 1]) in test_diff_real_dp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(x, append=[real(dp) :: 1]), [real(dp) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[real(dp) :: 1]) in test_diff_real_dp failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all_close(diff(reshape(A, [3,1]), n=1, dim=1), reshape([real(dp) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_real_dp failed")
        if (allocated(error)) return
        call check(error, all_close(diff(A, n=1, dim=2), reshape([real(dp) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_real_dp failed")
        if (allocated(error)) return
        
        call check(error, all_close(diff(A, n=1, dim=2, prepend=reshape([real(dp) :: 1], [1, 1]), &
            append=reshape([real(dp) :: 2], [1, 1])), reshape([real(dp) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([real(dp) :: 1], [1, 1]), &
            &append=reshape([real(dp) :: 2], [1, 1])) in test_diff_real_dp failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_real_dp failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_real_dp failed")
        
    end subroutine test_diff_real_dp
    
    subroutine test_diff_int_int8(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int8) :: x(6) = [integer(int8) :: 0, 5, 15, 30, 50, 75]
        integer(int8) :: A(1, 3) = reshape([integer(int8) :: 1, 3, 5], [1, 3])
        integer(int8) :: B(2) = [integer(int8) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all(diff(x) == [integer(int8) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_int_int8 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=0) == x), &
            "diff(<rank-1>, n=0) in test_diff_int_int8 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=2) == [integer(int8) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_int_int8 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(x, prepend=[integer(int8) :: 1]) == [integer(int8) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[integer(int8) :: 1]) in test_diff_int_int8 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, append=[integer(int8) :: 1]) == [integer(int8) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[integer(int8) :: 1]) in test_diff_int_int8 failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all(diff(reshape(A, [3,1]), n=1, dim=1) == reshape([integer(int8) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_int_int8 failed")
        if (allocated(error)) return
        call check(error, all(diff(A, n=1, dim=2) == reshape([integer(int8) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_int_int8 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(A, n=1, dim=2, prepend=reshape([integer(int8) :: 1], [1, 1]), &
            append=reshape([integer(int8) :: 2], [1, 1])) == reshape([integer(int8) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([integer(int8) :: 1], [1, 1]), &
            &append=reshape([integer(int8) :: 2], [1, 1])) in test_diff_int_int8 failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_int_int8 failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_int_int8 failed")
        
    end subroutine test_diff_int_int8
    subroutine test_diff_int_int16(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int16) :: x(6) = [integer(int16) :: 0, 5, 15, 30, 50, 75]
        integer(int16) :: A(1, 3) = reshape([integer(int16) :: 1, 3, 5], [1, 3])
        integer(int16) :: B(2) = [integer(int16) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all(diff(x) == [integer(int16) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_int_int16 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=0) == x), &
            "diff(<rank-1>, n=0) in test_diff_int_int16 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=2) == [integer(int16) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_int_int16 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(x, prepend=[integer(int16) :: 1]) == [integer(int16) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[integer(int16) :: 1]) in test_diff_int_int16 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, append=[integer(int16) :: 1]) == [integer(int16) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[integer(int16) :: 1]) in test_diff_int_int16 failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all(diff(reshape(A, [3,1]), n=1, dim=1) == reshape([integer(int16) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_int_int16 failed")
        if (allocated(error)) return
        call check(error, all(diff(A, n=1, dim=2) == reshape([integer(int16) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_int_int16 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(A, n=1, dim=2, prepend=reshape([integer(int16) :: 1], [1, 1]), &
            append=reshape([integer(int16) :: 2], [1, 1])) == reshape([integer(int16) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([integer(int16) :: 1], [1, 1]), &
            &append=reshape([integer(int16) :: 2], [1, 1])) in test_diff_int_int16 failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_int_int16 failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_int_int16 failed")
        
    end subroutine test_diff_int_int16
    subroutine test_diff_int_int32(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: x(6) = [integer(int32) :: 0, 5, 15, 30, 50, 75]
        integer(int32) :: A(1, 3) = reshape([integer(int32) :: 1, 3, 5], [1, 3])
        integer(int32) :: B(2) = [integer(int32) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all(diff(x) == [integer(int32) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_int_int32 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=0) == x), &
            "diff(<rank-1>, n=0) in test_diff_int_int32 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=2) == [integer(int32) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_int_int32 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(x, prepend=[integer(int32) :: 1]) == [integer(int32) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[integer(int32) :: 1]) in test_diff_int_int32 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, append=[integer(int32) :: 1]) == [integer(int32) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[integer(int32) :: 1]) in test_diff_int_int32 failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all(diff(reshape(A, [3,1]), n=1, dim=1) == reshape([integer(int32) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_int_int32 failed")
        if (allocated(error)) return
        call check(error, all(diff(A, n=1, dim=2) == reshape([integer(int32) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_int_int32 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(A, n=1, dim=2, prepend=reshape([integer(int32) :: 1], [1, 1]), &
            append=reshape([integer(int32) :: 2], [1, 1])) == reshape([integer(int32) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([integer(int32) :: 1], [1, 1]), &
            &append=reshape([integer(int32) :: 2], [1, 1])) in test_diff_int_int32 failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_int_int32 failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_int_int32 failed")
        
    end subroutine test_diff_int_int32
    subroutine test_diff_int_int64(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: x(6) = [integer(int64) :: 0, 5, 15, 30, 50, 75]
        integer(int64) :: A(1, 3) = reshape([integer(int64) :: 1, 3, 5], [1, 3])
        integer(int64) :: B(2) = [integer(int64) :: 1, 2]
        
        !> rank-1 diff
        call check(error, all(diff(x) == [integer(int64) :: 5, 10, 15, 20, 25]), &
            "diff(<rank-1>) in test_diff_int_int64 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=0) == x), &
            "diff(<rank-1>, n=0) in test_diff_int_int64 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, n=2) == [integer(int64) :: 5, 5, 5, 5]), &
            "diff(<rank-1>, n=2) in test_diff_int_int64 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(x, prepend=[integer(int64) :: 1]) == [integer(int64) :: -1, 5, 10, 15, 20, 25]), &
            "diff(<rank-1>, prepend=[integer(int64) :: 1]) in test_diff_int_int64 failed")
        if (allocated(error)) return
        call check(error, all(diff(x, append=[integer(int64) :: 1]) == [integer(int64) :: 5, 10, 15, 20, 25, -74]), &
            "diff(<rank-1>, append=[integer(int64) :: 1]) in test_diff_int_int64 failed")
        if (allocated(error)) return
        
        !> rank-2 diff
        call check(error, all(diff(reshape(A, [3,1]), n=1, dim=1) == reshape([integer(int64) :: 2, 2], [2, 1])), &
            "diff(<rank-2>, n=1, dim=1) in test_diff_int_int64 failed")
        if (allocated(error)) return
        call check(error, all(diff(A, n=1, dim=2) == reshape([integer(int64) :: 2, 2], [1, 2])), &
            "diff(<rank-2>, n=1, dim=2) in test_diff_int_int64 failed")
        if (allocated(error)) return
        
        call check(error, all(diff(A, n=1, dim=2, prepend=reshape([integer(int64) :: 1], [1, 1]), &
            append=reshape([integer(int64) :: 2], [1, 1])) == reshape([integer(int64) :: 0, 2, 2, -3], [1, 4])), &
            "diff(<rank-2>, n=1, dim=2, prepend=reshape([integer(int64) :: 1], [1, 1]), &
            &append=reshape([integer(int64) :: 2], [1, 1])) in test_diff_int_int64 failed")
        if (allocated(error)) return
        
        !> size(B, dim) <= n
        call check(error, size(diff(B, 2)), 0, "size(diff(B, 2)) in test_diff_int_int64 failed")
        if (allocated(error)) return
        call check(error, size(diff(B, 3)), 0, "size(diff(B, 3)) in test_diff_int_int64 failed")
        
    end subroutine test_diff_int_int64
    
    subroutine test_arange_real_sp(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all_close(arange(3.0_sp), [1.0_sp, 2.0_sp, 3.0_sp]), &
                   "all(arange(3.0_sp), [1.0_sp,2.0_sp,3.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(-1.0_sp), [1.0_sp, 0.0_sp, -1.0_sp]), &
                   "all_close(arange(-1.0_sp), [1.0_sp,0.0_sp,-1.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_sp, 2.0_sp), [0.0_sp, 1.0_sp, 2.0_sp]), &
                   "all_close(arange(0.0_sp,2.0_sp), [0.0_sp,1.0_sp,2.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_sp, -1.0_sp), [1.0_sp, 0.0_sp, -1.0_sp]), &
                   "all_close(arange(1.0_sp,-1.0_sp), [1.0_sp,0.0_sp,-1.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_sp, 1.0_sp), [1.0_sp]), &
                   "all_close(arange(1.0_sp,1.0_sp), [1.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_sp, 2.0_sp, 2.0_sp), [0.0_sp, 2.0_sp]), &
                   "all_close(arange(0.0_sp,2.0_sp,2.0_sp), [0.0_sp,2.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_sp, -1.0_sp, 2.0_sp), [1.0_sp, -1.0_sp]), &
                   "all_close(arange(1.0_sp,-1.0_sp,2.0_sp), [1.0_sp,-1.0_sp]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all_close(arange(0.0_sp, 2.0_sp, -2.0_sp), [0.0_sp, 2.0_sp]),  &
                   "all_close(arange(0.0_sp,2.0_sp,-2.0_sp), [0.0_sp,2.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_sp, -1.0_sp, -2.0_sp), [1.0_sp, -1.0_sp]), &
                   "all_close(arange(1.0_sp,-1.0_sp,-2.0_sp), [1.0_sp,-1.0_sp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_sp, 2.0_sp, 0.0_sp), [0.0_sp,1.0_sp,2.0_sp]), &
                   "all_close(arange(0.0_sp, 2.0_sp, 0.0_sp), [0.0_sp,1.0_sp,2.0_sp]) failed.")
    end subroutine test_arange_real_sp
    subroutine test_arange_real_dp(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all_close(arange(3.0_dp), [1.0_dp, 2.0_dp, 3.0_dp]), &
                   "all(arange(3.0_dp), [1.0_dp,2.0_dp,3.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(-1.0_dp), [1.0_dp, 0.0_dp, -1.0_dp]), &
                   "all_close(arange(-1.0_dp), [1.0_dp,0.0_dp,-1.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_dp, 2.0_dp), [0.0_dp, 1.0_dp, 2.0_dp]), &
                   "all_close(arange(0.0_dp,2.0_dp), [0.0_dp,1.0_dp,2.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_dp, -1.0_dp), [1.0_dp, 0.0_dp, -1.0_dp]), &
                   "all_close(arange(1.0_dp,-1.0_dp), [1.0_dp,0.0_dp,-1.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_dp, 1.0_dp), [1.0_dp]), &
                   "all_close(arange(1.0_dp,1.0_dp), [1.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_dp, 2.0_dp, 2.0_dp), [0.0_dp, 2.0_dp]), &
                   "all_close(arange(0.0_dp,2.0_dp,2.0_dp), [0.0_dp,2.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_dp, -1.0_dp, 2.0_dp), [1.0_dp, -1.0_dp]), &
                   "all_close(arange(1.0_dp,-1.0_dp,2.0_dp), [1.0_dp,-1.0_dp]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all_close(arange(0.0_dp, 2.0_dp, -2.0_dp), [0.0_dp, 2.0_dp]),  &
                   "all_close(arange(0.0_dp,2.0_dp,-2.0_dp), [0.0_dp,2.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(1.0_dp, -1.0_dp, -2.0_dp), [1.0_dp, -1.0_dp]), &
                   "all_close(arange(1.0_dp,-1.0_dp,-2.0_dp), [1.0_dp,-1.0_dp]) failed.")
        if (allocated(error)) return
        call check(error, all_close(arange(0.0_dp, 2.0_dp, 0.0_dp), [0.0_dp,1.0_dp,2.0_dp]), &
                   "all_close(arange(0.0_dp, 2.0_dp, 0.0_dp), [0.0_dp,1.0_dp,2.0_dp]) failed.")
    end subroutine test_arange_real_dp
    
    subroutine test_arange_int_int8(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all(arange(3_int8) == [1_int8, 2_int8, 3_int8]), &
                   "all(arange(3_int8) == [1_int8,2_int8,3_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(-1_int8) == [1_int8, 0_int8, -1_int8]), &
                   "all(arange(-1_int8) == [1_int8,0_int8,-1_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int8, 2_int8) == [0_int8, 1_int8, 2_int8]), &
                   "all(arange(0_int8,2_int8) == [0_int8,1_int8,2_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int8, -1_int8) == [1_int8, 0_int8, -1_int8]), &
                   "all(arange(1_int8,-1_int8) == [1_int8,0_int8,-1_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int8, 1_int8) == [1_int8]), &
                   "all(arange(1_int8,1_int8) == [1_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int8, 2_int8, 2_int8) == [0_int8, 2_int8]), &
                   "all(arange(0_int8,2_int8,2_int8) == [0_int8,2_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int8, -1_int8, 2_int8) == [1_int8, -1_int8]), &
                   "all(arange(1_int8,-1_int8,2_int8) == [1_int8,-1_int8]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all(arange(0_int8, 2_int8, -2_int8) == [0_int8, 2_int8]),  &
                   "all(arange(0_int8,2_int8,2_int8) == [0_int8,2_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int8, -1_int8, -2_int8) == [1_int8, -1_int8]), &
                   "all(arange(1_int8,-1_int8,2_int8) == [1_int8,-1_int8]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int8, 2_int8, 0_int8) == [0_int8, 1_int8, 2_int8]), &
                   "all(arange(0_int8,2_int8,0_int8) == [0_int8,1_int8,2_int8]) failed.")
    
    end subroutine test_arange_int_int8
    subroutine test_arange_int_int16(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all(arange(3_int16) == [1_int16, 2_int16, 3_int16]), &
                   "all(arange(3_int16) == [1_int16,2_int16,3_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(-1_int16) == [1_int16, 0_int16, -1_int16]), &
                   "all(arange(-1_int16) == [1_int16,0_int16,-1_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int16, 2_int16) == [0_int16, 1_int16, 2_int16]), &
                   "all(arange(0_int16,2_int16) == [0_int16,1_int16,2_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int16, -1_int16) == [1_int16, 0_int16, -1_int16]), &
                   "all(arange(1_int16,-1_int16) == [1_int16,0_int16,-1_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int16, 1_int16) == [1_int16]), &
                   "all(arange(1_int16,1_int16) == [1_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int16, 2_int16, 2_int16) == [0_int16, 2_int16]), &
                   "all(arange(0_int16,2_int16,2_int16) == [0_int16,2_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int16, -1_int16, 2_int16) == [1_int16, -1_int16]), &
                   "all(arange(1_int16,-1_int16,2_int16) == [1_int16,-1_int16]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all(arange(0_int16, 2_int16, -2_int16) == [0_int16, 2_int16]),  &
                   "all(arange(0_int16,2_int16,2_int16) == [0_int16,2_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int16, -1_int16, -2_int16) == [1_int16, -1_int16]), &
                   "all(arange(1_int16,-1_int16,2_int16) == [1_int16,-1_int16]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int16, 2_int16, 0_int16) == [0_int16, 1_int16, 2_int16]), &
                   "all(arange(0_int16,2_int16,0_int16) == [0_int16,1_int16,2_int16]) failed.")
    
    end subroutine test_arange_int_int16
    subroutine test_arange_int_int32(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all(arange(3_int32) == [1_int32, 2_int32, 3_int32]), &
                   "all(arange(3_int32) == [1_int32,2_int32,3_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(-1_int32) == [1_int32, 0_int32, -1_int32]), &
                   "all(arange(-1_int32) == [1_int32,0_int32,-1_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int32, 2_int32) == [0_int32, 1_int32, 2_int32]), &
                   "all(arange(0_int32,2_int32) == [0_int32,1_int32,2_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int32, -1_int32) == [1_int32, 0_int32, -1_int32]), &
                   "all(arange(1_int32,-1_int32) == [1_int32,0_int32,-1_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int32, 1_int32) == [1_int32]), &
                   "all(arange(1_int32,1_int32) == [1_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int32, 2_int32, 2_int32) == [0_int32, 2_int32]), &
                   "all(arange(0_int32,2_int32,2_int32) == [0_int32,2_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int32, -1_int32, 2_int32) == [1_int32, -1_int32]), &
                   "all(arange(1_int32,-1_int32,2_int32) == [1_int32,-1_int32]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all(arange(0_int32, 2_int32, -2_int32) == [0_int32, 2_int32]),  &
                   "all(arange(0_int32,2_int32,2_int32) == [0_int32,2_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int32, -1_int32, -2_int32) == [1_int32, -1_int32]), &
                   "all(arange(1_int32,-1_int32,2_int32) == [1_int32,-1_int32]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int32, 2_int32, 0_int32) == [0_int32, 1_int32, 2_int32]), &
                   "all(arange(0_int32,2_int32,0_int32) == [0_int32,1_int32,2_int32]) failed.")
    
    end subroutine test_arange_int_int32
    subroutine test_arange_int_int64(error)
        type(error_type), allocatable, intent(out) :: error
        
        ! Normal
        call check(error, all(arange(3_int64) == [1_int64, 2_int64, 3_int64]), &
                   "all(arange(3_int64) == [1_int64,2_int64,3_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(-1_int64) == [1_int64, 0_int64, -1_int64]), &
                   "all(arange(-1_int64) == [1_int64,0_int64,-1_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int64, 2_int64) == [0_int64, 1_int64, 2_int64]), &
                   "all(arange(0_int64,2_int64) == [0_int64,1_int64,2_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int64, -1_int64) == [1_int64, 0_int64, -1_int64]), &
                   "all(arange(1_int64,-1_int64) == [1_int64,0_int64,-1_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int64, 1_int64) == [1_int64]), &
                   "all(arange(1_int64,1_int64) == [1_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int64, 2_int64, 2_int64) == [0_int64, 2_int64]), &
                   "all(arange(0_int64,2_int64,2_int64) == [0_int64,2_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int64, -1_int64, 2_int64) == [1_int64, -1_int64]), &
                   "all(arange(1_int64,-1_int64,2_int64) == [1_int64,-1_int64]) failed.")
        if (allocated(error)) return
        
        ! Not recommended
        call check(error, all(arange(0_int64, 2_int64, -2_int64) == [0_int64, 2_int64]),  &
                   "all(arange(0_int64,2_int64,2_int64) == [0_int64,2_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(1_int64, -1_int64, -2_int64) == [1_int64, -1_int64]), &
                   "all(arange(1_int64,-1_int64,2_int64) == [1_int64,-1_int64]) failed.")
        if (allocated(error)) return
        call check(error, all(arange(0_int64, 2_int64, 0_int64) == [0_int64, 1_int64, 2_int64]), &
                   "all(arange(0_int64,2_int64,0_int64) == [0_int64,1_int64,2_int64]) failed.")
    
    end subroutine test_arange_int_int64
        

end module test_stdlib_math

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stdlib_math, only : collect_stdlib_math
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stdlib-math", collect_stdlib_math) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
