message(STATUS "Building tests")
foreach(execid
  vertex
  edge
  graph
  )
  add_executable(test_${execid} test_${execid}.f90)
  # # Specify the include directories
  target_include_directories(test_${execid} PRIVATE "${CMAKE_BUILD_PREFIX}" "${CMAKE_BUILD_PREFIX}/mod")

  target_link_libraries(test_${execid} PRIVATE graphstruc)

  add_test(NAME test_${execid} COMMAND test_${execid})
endforeach()
