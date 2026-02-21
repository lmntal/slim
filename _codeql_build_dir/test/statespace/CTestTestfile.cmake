# CMake generated Testfile for 
# Source directory: /home/runner/work/slim/slim/test/statespace
# Build directory: /home/runner/work/slim/slim/_codeql_build_dir/test/statespace
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(statespace_advanced "/home/runner/work/slim/slim/_codeql_build_dir/test/statespace/advanced_skip_wrapper.sh")
set_tests_properties(statespace_advanced PROPERTIES  ENVIRONMENT "SLIM_BINARY=/home/runner/work/slim/slim/_codeql_build_dir/src/slim;LMNTAL_HOME=" SKIP_RETURN_CODE "77" WORKING_DIRECTORY "/home/runner/work/slim/slim/test/statespace" _BACKTRACE_TRIPLES "/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;62;add_test;/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;0;")
add_test(statespace_basic "/home/runner/work/slim/slim/test/statespace/testsuite/basic/check.sh")
set_tests_properties(statespace_basic PROPERTIES  DEPENDS "/home/runner/work/slim/slim/test/statespace/testsuite/basic/nd.il;/home/runner/work/slim/slim/test/statespace/testsuite/basic/nd2.il" ENVIRONMENT "SLIM_BINARY=/home/runner/work/slim/slim/_codeql_build_dir/src/slim;LMNTAL_HOME=" WORKING_DIRECTORY "/home/runner/work/slim/slim/test/statespace" _BACKTRACE_TRIPLES "/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;92;add_test;/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;0;")
add_test(statespace_hyperlink "/home/runner/work/slim/slim/test/statespace/testsuite/hyperlink/check.sh")
set_tests_properties(statespace_hyperlink PROPERTIES  DEPENDS "/home/runner/work/slim/slim/test/statespace/testsuite/hyperlink/cycle.il;/home/runner/work/slim/slim/test/statespace/testsuite/hyperlink/lambda_nd-hl7.il;/home/runner/work/slim/slim/test/statespace/testsuite/hyperlink/mc-hl.il" ENVIRONMENT "SLIM_BINARY=/home/runner/work/slim/slim/_codeql_build_dir/src/slim;LMNTAL_HOME=" WORKING_DIRECTORY "/home/runner/work/slim/slim/test/statespace" _BACKTRACE_TRIPLES "/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;92;add_test;/home/runner/work/slim/slim/test/statespace/CMakeLists.txt;0;")
