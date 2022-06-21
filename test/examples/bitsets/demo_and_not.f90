program demo_and_not
use stdlib_bitsets
type(bitset_large) :: set0, set1
call set0 % init(166)
call set1 % init(166)
call and_not( set0, set1 ) ! none none
if ( none(set0) ) write(*,*) 'First test of AND_NOT worked.'
call set0 % not()
call and_not( set0, set1 ) ! all none
if ( all(set0) ) write(*,*) 'Second test of AND_NOT worked.'
call set0 % not()
call set1 % not()
call and_not( set0, set1 ) ! none all
if ( none(set0) ) write(*,*) 'Third test of AND_NOT worked.'
call set0 % not()
call and_not( set0, set1 ) ! all all
if ( none(set0) ) write(*,*) 'Fourth test of AND_NOT worked.'
end program demo_and_not
