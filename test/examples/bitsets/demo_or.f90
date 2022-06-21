program demo_or
use stdlib_bitsets
type(bitset_large) :: set0, set1
call set0 % init(166)
call set1 % init(166)
call or( set0, set1 ) ! none none
if ( none(set0) ) write(*,*) 'First test of OR worked.'
call set0 % not()
call or( set0, set1 ) ! all none
if ( all(set0) ) write(*,*) 'Second test of OR worked.'
call set0 % not()
call set1 % not()
call or( set0, set1 ) ! none all
if ( all(set0) ) write(*,*) 'Third test of OR worked.'
call set0 % not()
call or( set0, set1 ) ! all all
if ( all(set0) ) write(*,*) 'Fourth test of OR worked.'
end program demo_or
