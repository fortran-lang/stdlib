gfortran -c ../../src/stdlib_child_list.f90
gfortran -c ../../src/stdlib_linked_list.f90
rem gfortran -c linked_list_aux.f90

gfortran -o example_linked_size example_linked_size.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_clear example_linked_clear.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_get example_linked_get.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_insert example_linked_insert.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_replace example_linked_replace.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_remove example_linked_remove.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_push example_linked_push.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_pop example_linked_pop.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_reverse example_linked_reverse.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_concat example_linked_concat.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_absorb example_linked_absorb.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_slice example_linked_slice.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_linked_splice example_linked_splice.f90 stdlib_linked_list.o stdlib_child_list.o
