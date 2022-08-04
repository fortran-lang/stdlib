program example_strip
  use stdlib_ascii, only: TAB, VT, LF, CR, FF
  use stdlib_strings, only: strip
  implicit none
  print'(a)', strip("   hello   ")             ! "hello"
  print'(a)', strip(TAB//"goodbye"//CR//LF)    ! "goodbye"
  print'(a)', strip(" "//TAB//LF//VT//FF//CR)  ! ""
  print'(a)', strip("  !  ")//"!"              ! "!!"
  print'(a)', strip("Hello")                   ! "Hello"
end program example_strip
