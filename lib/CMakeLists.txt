# Library files - these are typically data/config files
# Install library files to appropriate location following Unix conventions

# Install all .il library files to share/slim/lib (matches arch.h expectations)
file(GLOB LIB_IL_FILES "*.il")
install(FILES ${LIB_IL_FILES} DESTINATION share/slim/lib)

# Install all .lmn library files to share/slim/lib for reference
file(GLOB LIB_LMN_FILES "*.lmn")
install(FILES ${LIB_LMN_FILES} DESTINATION share/slim/lib)

# Create ext directory as well
install(DIRECTORY DESTINATION share/slim/ext)