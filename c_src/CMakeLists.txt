add_library(cute_net STATIC "cute_net.c")
if (LINUX)
	target_compile_options(cute_net PRIVATE  "-fPIC" "-Wno-unused-variable" "-Wno-strict-prototypes" "-Wno-unused-but-set-variable")
endif ()

add_library(cn_crypto SHARED "cn_crypto.c")
target_include_directories(cn_crypto PRIVATE ${OTP_INCLUDE_DIR})
target_link_libraries(cn_crypto PRIVATE cute_net)
set_target_properties(cn_crypto PROPERTIES
    PREFIX ""
)
set_target_properties(cn_crypto PROPERTIES C_VISIBILITY_PRESET hidden)
if (NOT MSVC)
	target_link_options(cn_crypto PRIVATE
		-Wl,--exclude-libs,ALL
		-Wl,--no-whole-archive
	)
endif ()
