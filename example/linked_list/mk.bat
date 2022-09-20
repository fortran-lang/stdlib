gfortran -c ../../src/stdlib_child_list.f90
gfortran -c ../../src/stdlib_linked_list.f90
rem gfortran -c linked_list_aux.f90

gfortran -o example_size example_size.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_clear example_clear.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_get example_get.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_insert example_insert.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_replace example_replace.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_remove example_remove.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_push example_push.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_pop example_pop.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_reverse example_reverse.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_concat example_concat.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_absorb example_absorb.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_slice example_slice.f90 stdlib_linked_list.o stdlib_child_list.o
gfortran -o example_splice example_splice.f90 stdlib_linked_list.o stdlib_child_list.o
