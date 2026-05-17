cmake_minimum_required(VERSION 3.3.2)

project(extlib
    LANGUAGES Fortran
    VERSION 0.1.0
)

set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

add_subdirectory(src)

if(${BUILD_TESTING})
    include(CTest)
    add_subdirectory(tests)
endif()

install(TARGETS extlib
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)
install(DIRECTORY "${PROJECT_BINARY_DIR}/include"
    DESTINATION .
    FILES_MATCHING PATTERN "*.mod"
)
