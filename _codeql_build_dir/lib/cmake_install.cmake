# Install script for directory: /home/runner/work/slim/slim/lib

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/slim/lib" TYPE FILE FILES
    "/home/runner/work/slim/slim/lib/array.lmn"
    "/home/runner/work/slim/slim/lib/array2D.lmn"
    "/home/runner/work/slim/slim/lib/atom.lmn"
    "/home/runner/work/slim/slim/lib/boolean.lmn"
    "/home/runner/work/slim/slim/lib/deque.lmn"
    "/home/runner/work/slim/slim/lib/float.lmn"
    "/home/runner/work/slim/slim/lib/functional.lmn"
    "/home/runner/work/slim/slim/lib/if.lmn"
    "/home/runner/work/slim/slim/lib/integer.lmn"
    "/home/runner/work/slim/slim/lib/io.lmn"
    "/home/runner/work/slim/slim/lib/list.lmn"
    "/home/runner/work/slim/slim/lib/map.lmn"
    "/home/runner/work/slim/slim/lib/mell.lmn"
    "/home/runner/work/slim/slim/lib/membrane.lmn"
    "/home/runner/work/slim/slim/lib/nd_conf.lmn"
    "/home/runner/work/slim/slim/lib/nlmem.lmn"
    "/home/runner/work/slim/slim/lib/queue.lmn"
    "/home/runner/work/slim/slim/lib/react_rule.lmn"
    "/home/runner/work/slim/slim/lib/seq.lmn"
    "/home/runner/work/slim/slim/lib/set.lmn"
    "/home/runner/work/slim/slim/lib/state_space.lmn"
    "/home/runner/work/slim/slim/lib/str.lmn"
    "/home/runner/work/slim/slim/lib/string.lmn"
    "/home/runner/work/slim/slim/lib/system_ruleset.lmn"
    "/home/runner/work/slim/slim/lib/time.lmn"
    "/home/runner/work/slim/slim/lib/unit_test.lmn"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/slim/ext" TYPE DIRECTORY FILES "")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/runner/work/slim/slim/_codeql_build_dir/lib/install_local_manifest.txt"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
