if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(
    CMAKE_Fortran_FLAGS_INIT
    "-fimplicit-none"
    "-ffree-line-length-132"
  )
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-Wall"
    "-Wextra"
    "-Wimplicit-procedure"
    "-std=f2018"
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set(
    CMAKE_Fortran_FLAGS_INIT
  )
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
  )
  if(WIN32)
    set(
      CMAKE_Fortran_FLAGS_DEBUG_INIT
      "/stand:f18"
      "/warn:declarations,general,usage,interfaces,unused"
    )
  else()
    set(
      CMAKE_Fortran_FLAGS_DEBUG_INIT
      "-stand f18"
      "-warn declarations,general,usage,interfaces,unused"
    )
  endif()
else()
  set(
    CMAKE_Fortran_FLAGS_INIT
  )
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
  )
endif()
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_INIT "${CMAKE_Fortran_FLAGS_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_RELEASE_INIT "${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_DEBUG_INIT "${CMAKE_Fortran_FLAGS_DEBUG_INIT}")
