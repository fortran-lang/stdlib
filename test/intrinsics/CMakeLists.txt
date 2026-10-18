set(
  fppFiles
  "test_intrinsics.fypp"
  "test_sum_mask.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(intrinsics)

target_sources(test_intrinsics PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/test_sum_mask.f90)
