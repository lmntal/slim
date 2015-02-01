#include "unit_test.h"

void test_main() {
  CU_pSuite tree_suite;

  CU_initialize_registry();

  // add your test suite here
  // sort_suite = CU_add_suite("Sort", NULL, NULL);
  // CU_add_test(sort_suite, "test_001", test_sort_001);
  // CU_add_test(sort_suite, "test_002", test_sort_002);
  // CU_add_test(sort_suite, "test_003", test_sort_003);
  // CU_add_test(sort_suite, "test_004", test_sort_004);
  // CU_add_test(sort_suite, "test_005", test_sort_005);

  tree_suite = CU_add_suite("Tree Compression", test_tree_init, test_tree_clean);
  CU_add_test(tree_suite, "tree_test_001", test_tree_001);
  CU_add_test(tree_suite, "tree_test_002", test_tree_002);
  CU_add_test(tree_suite, "tree_test_003", test_tree_003);

  CU_basic_run_tests();
  CU_cleanup_registry();

  return;
}
