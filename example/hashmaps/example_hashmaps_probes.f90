program example_probes
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  integer :: nprobes
  call map%init(fnv_1_hasher)
  nprobes = map%map_probes()
  print *, "Initial probes =  ", nprobes
end program example_probes
