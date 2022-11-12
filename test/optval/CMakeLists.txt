set(
  fppFiles
  "test_optval.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(optval)
# prevent false positive (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95446)
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  set_source_files_properties("test_optval.f90" PROPERTIES COMPILE_FLAGS "-Wno-error=pedantic")
endif()
