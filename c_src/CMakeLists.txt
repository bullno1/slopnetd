add_executable(cn_server "cn_server.c" "libs.c")
target_link_libraries(cn_server)
find_library(MATH_LIBRARY m)
if(MATH_LIBRARY)
    target_link_libraries(cn_server PUBLIC ${MATH_LIBRARY})
endif()

add_library(snetd_dummy SHARED "snetd_dummy.c")
set_target_properties(snetd_dummy PROPERTIES
    PREFIX ""
)
set_target_properties(snetd_dummy PROPERTIES C_VISIBILITY_PRESET hidden)
if (NOT MSVC)
	target_link_options(snetd_dummy PRIVATE
		-Wl,--exclude-libs,ALL
		-Wl,--no-whole-archive
	)
endif ()

find_package(OpenSSL REQUIRED)

add_library(snetd_crypto SHARED "snetd_crypto.c")
target_include_directories(snetd_crypto PRIVATE ${OTP_INCLUDE_DIR} ${OPENSSL_INCLUDE_DIR})
target_link_libraries(snetd_crypto PRIVATE OpenSSL::Crypto)
set_target_properties(snetd_crypto PROPERTIES
	PREFIX ""
	C_VISIBILITY_PRESET hidden
)
if (NOT MSVC)
	target_link_options(snetd_crypto PRIVATE
		-Wl,--exclude-libs,ALL
		-Wl,--no-whole-archive
	)
endif ()
