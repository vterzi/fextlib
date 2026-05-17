file(GLOB sources "*.f90" "*.F90")
foreach(source ${sources})
    get_filename_component(label "${source}" NAME_WE)
    add_executable("${label}" "${source}")
    target_link_libraries("${label}" PRIVATE extlib)
    target_include_directories(
        "${label}" PRIVATE "${PROJECT_BINARY_DIR}/include"
    )
    add_test(NAME "${label}" COMMAND "${label}")
endforeach()
