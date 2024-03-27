set(_NAME "minizip")
set(_URL "https://github.com/zlib-ng/minizip-ng")
set(_TAG "4.0.4")

message(STATUS "Retrieving ${_NAME} from ${_URL}")
include(FetchContent)
FetchContent_Declare(
    ${_NAME}
    GIT_REPOSITORY ${_URL}
    GIT_TAG ${_TAG}
)
FetchContent_MakeAvailable(${_NAME})
add_library("${_NAME}::${_NAME}" INTERFACE IMPORTED)
target_link_libraries("${_NAME}::${_NAME}" INTERFACE "${_NAME}")
