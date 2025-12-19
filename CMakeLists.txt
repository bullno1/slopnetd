cmake_minimum_required(VERSION 3.15)
project(slopnetd)

set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/priv/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/priv/bin)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS OFF)

if (MSVC)
	add_compile_options(/W3 /WX)
	add_compile_options(/wd4200) # Flexible array member is a standard feature since C99
	add_compile_options(/wd4324) # _Alignof is intentional
	add_compile_options(/wd4100) # Unreferenced parameter
	add_compile_options(/wd4459) # Hiding global definition
	add_compile_options(/experimental:c11atomics)  # Atomics
	add_compile_definitions(_CRT_SECURE_NO_WARNINGS)

	# Because CMake can't set proper flags even with CMAKE_INTERPROCEDURAL_OPTIMIZATION_Release
	set(RELEASE_COMPILER_FLAGS "/Gy;/Ob2;/GL;/Oi")
	set(RELEASE_LINKER_FLAGS "/LTCG")

	add_compile_options("$<$<CONFIG:RelWithDebInfo>:${RELEASE_COMPILER_FLAGS}>")
	add_link_options("$<$<CONFIG:RelWithDebInfo>:${RELEASE_LINKER_FLAGS}>")
else()
	add_compile_options(
		-Wall -Wextra -pedantic -Werror
		-Wno-unused-parameter
		-Wno-error=c23-extensions
	)
endif()

set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH "\${ORIGIN}")

execute_process(
	COMMAND erl -noshell -eval "io:format(\"~s\", [code:root_dir()])." -s init stop
	OUTPUT_VARIABLE OTP_ROOT
)
message("Found Erlang: ${OTP_ROOT}")
set(OTP_INCLUDE_DIR "${OTP_ROOT}/usr/include")
set(OTP_LIB_DIR "${OTP_ROOT}/usr/lib")

add_subdirectory(c_src)
