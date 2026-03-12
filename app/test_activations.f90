program test_activations
    use iso_fortran_env, only: dp => real64
    ! We use the PARENT module, not the submodule
    use stdlib_specialfunctions, only: sigmoid
    implicit none

    real(dp) :: x, y, expected
    integer :: pass_count

    print *, "=========================================="
    print *, " STARTING ACTIVATION FUNCTION LOGIC TEST "
    print *, "=========================================="

    pass_count = 0

    ! Test 1: Sigmoid(0) should be 0.5
    x = 0.0_dp
    expected = 0.5_dp
    y = sigmoid(x)
    
    if (abs(y - expected) < 1.0e-9_dp) then
        print *, "[PASS] Sigmoid(0.0) calculated correctly."
        pass_count = pass_count + 1
    else
        print *, "[FAIL] Sigmoid(0.0). Expected:", expected, "Got:", y
    end if

    ! Test 2: Sigmoid(1) should be ~0.731
    x = 1.0_dp
    expected = 0.7310585786300049_dp
    y = sigmoid(x)

    if (abs(y - expected) < 1.0e-9_dp) then
        print *, "[PASS] Sigmoid(1.0) calculated correctly."
        pass_count = pass_count + 1
    else
        print *, "[FAIL] Sigmoid(1.0). Expected:", expected, "Got:", y
    end if

    ! Test 3: Sigmoid(-1) should be ~0.268
    x = -1.0_dp
    expected = 0.2689414213699951_dp
    y = sigmoid(x)

    if (abs(y - expected) < 1.0e-9_dp) then
        print *, "[PASS] Sigmoid(-1.0) calculated correctly."
        pass_count = pass_count + 1
    else
        print *, "[FAIL] Sigmoid(-1.0). Expected:", expected, "Got:", y
    end if

    print *, "=========================================="
    if (pass_count == 3) then
        print *, "ALL TESTS PASSED! (3/3)"
    else
        print *, "SOME TESTS FAILED:", pass_count, "/ 3"
        error stop 1
    end if

end program test_activations
