cmake_minimum_required(VERSION 3.10)

project(dotfiles)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(link
  link.cpp
  util.cpp
  )

add_executable(unlink
  unlink.cpp
  util.cpp
  )
