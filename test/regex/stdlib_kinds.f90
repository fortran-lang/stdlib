module stdlib_kinds
  implicit none
  public
  integer, parameter :: sp = kind(1.0)
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: int32 = selected_int_kind(9)
end module stdlib_kinds
