! SPDX-Identifier: MIT

module cuckoohash_testing
    use stdlib_ascii, only : to_string
    use stdlib_container, only : container_type
    use stdlib_error, only : check
    use stdlib_map_cuckoohash, only : cuckoo_hash, new_cuckoo_hash, len
    implicit none

contains

    subroutine check_eq(actual, expected)
        integer, intent(in) :: actual
        integer, intent(in) :: expected
        logical :: stat
        character(len=:), allocatable :: message
        stat = actual == expected

        if (.not.stat) then
            message = "Expected '"//to_string(expected)//"' but got '"//to_string(actual)//"'"
        end if
        call check(stat, msg=message)
    end subroutine check_eq


    !> Tester for the cuckoo hash map implementation
    subroutine test_cuckoohash
        implicit none

        type(cuckoo_hash) :: hash
        type(container_type), pointer :: view

        call new_cuckoo_hash(hash, 10)
        call check_eq(len(hash), 0)

        call hash%get("tight-binding", view)
        call check(.not.associated(view))

        call hash%insert("tight-binding", view)
        call check(associated(view))
        call check_eq(len(hash), 1)

        call hash%insert("semi-empirics", view)
        call hash%insert("semi-empirics", view)
        call hash%insert("quantum-mechanics", view)
        call check_eq(len(hash), 3)

        call hash%drop("tight-binding")
        call check_eq(len(hash), 2)

        call hash%insert("talkative-backs", view)
        call hash%insert("thick-bounces", view)
        call hash%insert("tubby-broadcast", view)
        call hash%insert("these-bump", view)
        call hash%insert("tricky-broadcast", view)
        call hash%insert("thick-brush", view)
        call hash%insert("tart-brushes", view)
        call hash%insert("tragic-box", view)
        call hash%insert("thankful-bail", view)
        call hash%insert("trustworthy-balloon", view)
        call hash%insert("tame-balances", view)
        call hash%insert("tight-bumps", view)
        call hash%insert("tangible-bump", view)
        call hash%insert("tart-bother", view)
        call hash%insert("trivial-balloons", view)
        call hash%insert("twin-break", view)
        call hash%insert("tall-bandage", view)
        call hash%insert("twin-boxes", view)
        call hash%insert("tense-bounce", view)
        call hash%insert("thunderous-blast", view)
        call hash%insert("trivial-backs", view)
        call hash%insert("twin-board", view)
        call hash%insert("tremendous-benefits", view)
        call hash%insert("trained-breaks", view)
        call hash%insert("thorny-bank", view)
        call hash%insert("traumatic-bounces", view)
        call hash%insert("traumatic-bank", view)
        call hash%insert("thankful-bargain", view)

        call hash%get("tight-binding", view)
        call check(.not.associated(view))
        call hash%get("semi-empirics", view)
        call check(associated(view))
        call hash%get("quantum-mechanics", view)
        call check(associated(view))
        call hash%get("talkative-backs", view)
        call check(associated(view))
        call hash%get("thick-bounces", view)
        call check(associated(view))
        call hash%get("tubby-broadcast", view)
        call check(associated(view))
        call hash%get("these-bump", view)
        call check(associated(view))
        call hash%get("tricky-broadcast", view)
        call check(associated(view))
        call hash%get("thick-brush", view)
        call check(associated(view))
        call hash%get("tart-brushes", view)
        call check(associated(view))
        call hash%get("tragic-box", view)
        call check(associated(view))
        call hash%get("thankful-bail", view)
        call check(associated(view))
        call hash%get("trustworthy-balloon", view)
        call check(associated(view))
        call hash%get("tame-balances", view)
        call check(associated(view))
        call hash%get("tight-bumps", view)
        call check(associated(view))
        call hash%get("tangible-bump", view)
        call check(associated(view))
        call hash%get("tart-bother", view)
        call check(associated(view))
        call hash%get("trivial-balloons", view)
        call check(associated(view))
        call hash%get("twin-break", view)
        call check(associated(view))
        call hash%get("tall-bandage", view)
        call check(associated(view))
        call hash%get("twin-boxes", view)
        call check(associated(view))
        call hash%get("tense-bounce", view)
        call check(associated(view))
        call hash%get("thunderous-blast", view)
        call check(associated(view))
        call hash%get("trivial-backs", view)
        call check(associated(view))
        call hash%get("twin-board", view)
        call check(associated(view))
        call hash%get("tremendous-benefits", view)
        call check(associated(view))
        call hash%get("trained-breaks", view)
        call check(associated(view))
        call hash%get("thorny-bank", view)
        call check(associated(view))
        call hash%get("traumatic-bounces", view)
        call check(associated(view))
        call hash%get("traumatic-bank", view)
        call check(associated(view))
        call hash%get("thankful-bargain", view)
        call check(associated(view))

        call hash%clear
        call check_eq(len(hash), 0)

    end subroutine

end module cuckoohash_testing

program test_driver
    use cuckoohash_testing
    implicit none

    call test_cuckoohash

end program test_driver
