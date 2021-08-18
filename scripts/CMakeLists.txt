cmake_minimum_required(VERSION 3.10)

project(dotfiles)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Check that <filesystem> is supported

if (NOT USE_FS_POLYFILL)

  try_compile(FS_SUPPORTED ${CMAKE_BINARY_DIR}/fs-test ${CMAKE_SOURCE_DIR}/fs-test.cpp)

  if (FS_SUPPORTED)
    message("<filesystem> is natively supported")
  else()
    set(USE_FS_POLYFILL TRUE)
  endif()

endif()

if (USE_FS_POLYFILL)

  message("Using <filesystem> polyfill")
  add_compile_definitions(USE_FS_POLYFILL)

endif()

# Add executables

add_executable(link
  link.cpp
  fs.cpp
  util.cpp
  )

add_executable(unlink
  unlink.cpp
  fs.cpp
  util.cpp
  )
