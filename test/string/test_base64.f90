! SPDX-Identifier: MIT
module test_base64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_base64, only : base64_encode, base64_decode
    use stdlib_kinds, only : int8, int32, dp, lk
    implicit none

contains

    subroutine collect_base64(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("base64-known-vectors", test_known_vectors), &
            new_unittest("base64-decode-whitespace", test_decode_whitespace), &
            new_unittest("base64-decode-invalid", test_decode_invalid), &
            new_unittest("base64-roundtrip-int32", test_roundtrip_int32), &
            new_unittest("base64-roundtrip-real", test_roundtrip_real), &
            new_unittest("base64-roundtrip-complex", test_roundtrip_complex), &
            new_unittest("base64-roundtrip-logical", test_roundtrip_logical), &
            new_unittest("base64-rank0", test_rank0_encode) &
            ]
    end subroutine collect_base64

    subroutine test_known_vectors(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, base64_encode([int(77, int8), int(97, int8), int(110, int8)]) == "TWFu")
        if (allocated(error)) return
        call check(error, base64_encode([int(77, int8), int(97, int8)]) == "TWE=")
        if (allocated(error)) return
        call check(error, base64_encode([int(77, int8)]) == "TQ==")
        if (allocated(error)) return

        call check(error, base64_decode("TWFu") == "Man")
        if (allocated(error)) return
        call check(error, base64_decode("TWE=") == "Ma")
        if (allocated(error)) return
        call check(error, base64_decode("TQ==") == "M")
    end subroutine test_known_vectors

    subroutine test_decode_whitespace(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, base64_decode("T W" // new_line("a") // "Fu") == "Man")
    end subroutine test_decode_whitespace

    subroutine test_decode_invalid(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, base64_decode("abc") == "")
        if (allocated(error)) return
        call check(error, base64_decode("A===") == "")
        if (allocated(error)) return
        call check(error, base64_decode("AA=A") == "")
    end subroutine test_decode_invalid

    subroutine test_roundtrip_int32(error)
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: vals(4), got(4)
        character(len=:), allocatable :: enc, dec

        vals = [1_int32, -2_int32, 1024_int32, -4096_int32]

        enc = base64_encode(vals)
        dec = base64_decode(enc)
        got = transfer(dec, got)

        call check(error, all(got == vals))
    end subroutine test_roundtrip_int32

    subroutine test_roundtrip_real(error)
        type(error_type), allocatable, intent(out) :: error

        real(dp) :: vals(4), got(4)
        character(len=:), allocatable :: enc, dec

        vals = [1.5_dp, -2.25_dp, 0.125_dp, 9.0_dp]

        enc = base64_encode(vals)
        dec = base64_decode(enc)
        got = transfer(dec, got)

        call check(error, all(got == vals))
    end subroutine test_roundtrip_real

    subroutine test_roundtrip_complex(error)
        type(error_type), allocatable, intent(out) :: error

        complex(dp) :: vals(3), got(3)
        character(len=:), allocatable :: enc, dec

        vals = [cmplx(1.0_dp, 2.0_dp, dp), cmplx(-3.0_dp, 0.5_dp, dp), cmplx(0.0_dp, -4.0_dp, dp)]

        enc = base64_encode(vals)
        dec = base64_decode(enc)
        got = transfer(dec, got)

        call check(error, all(got == vals))
    end subroutine test_roundtrip_complex

    subroutine test_roundtrip_logical(error)
        type(error_type), allocatable, intent(out) :: error
        logical(lk) :: vals(5), got(5)
        character(len=:), allocatable :: enc, dec

        vals = [.true._lk, .false._lk, .true._lk, .true._lk, .false._lk]

        enc = base64_encode(vals)
        dec = base64_decode(enc)
        
        got = transfer(dec, got)

        call check(error, all((vals .neqv. .false._lk) .eqv. (got .neqv. .false._lk)))
    end subroutine test_roundtrip_logical

    subroutine test_rank0_encode(error)
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: v

        v = 42_int32
        call check(error, len(base64_encode(v)) > 0)
    end subroutine test_rank0_encode

end module test_base64


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_base64, only : collect_base64
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("base64", collect_base64) &
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
