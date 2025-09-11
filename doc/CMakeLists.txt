# Documentation
# Install man page and other documentation

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/slim.1)
    install(FILES slim.1 DESTINATION share/man/man1)
endif()

# Install other documentation files if they exist
file(GLOB DOC_FILES "*.md" "*.txt" "*.rst")
if(DOC_FILES)
    install(FILES ${DOC_FILES} DESTINATION share/doc/slim)
endif()