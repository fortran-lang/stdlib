program example_probes
  use stdlib_hashmaps, only: chaining_hashmap_type
  implicit none
  type(chaining_hashmap_type) :: map
  integer :: nprobes
  call map%init()
  nprobes = map%map_probes()
  print *, "Initial probes =  ", nprobes
end program example_probes
