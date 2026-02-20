#!/usr/bin/perl
######################################
#                                    #
# Java LMNtal system checker on Perl #
#                                    #
######################################
# AUTHOR : kudo
# LAST UPDATE : 2006/05/22

# This is LMNtal system checker.

use Cwd;
use File::Basename;
use File::Path qw(make_path);
$pwd = Cwd::getcwd();

$lmntal_runtime = $ARGV[0] || $ENV{SLIM_BINARY} || "/home/ueda/slim/build/src/slim";  # First argument is the slim binary path
$lmntal_home = $ENV{LMNTAL_HOME};
$count = 1;
$test_failed = 0;
$options = $ENV{slim_CHECK_OPTIONS};

# Generate .il file from .lmntest using create_testdata.awk.
# Output goes to $TEST_BUILD_DIR/generated/<suite>/<case>.il when TEST_BUILD_DIR is set,
# to avoid conflicts with pre-existing .il files in the source tree.
# Returns the path of the .il file to run SLIM on.
sub generate_il_file {
    my ($source_il) = @_;
    my $lmntest_file = $source_il;
    $lmntest_file =~ s/\.il$/.lmntest/;

    # If no .lmntest, use the source .il directly (no transformation needed)
    return $source_il unless -f $lmntest_file;

    # Determine output path
    my $build_dir = $ENV{TEST_BUILD_DIR};
    my $output_il;
    if ($build_dir) {
        my $suite = basename(dirname($source_il));
        my $case  = basename($source_il);
        $output_il = "$build_dir/generated/$suite/$case";
    } else {
        $output_il = $source_il;
    }

    if (!-f $output_il || -M $output_il > -M $lmntest_file) {
        # Create output directory if needed
        my $output_dir = dirname($output_il);
        make_path($output_dir) unless -d $output_dir;

        my $script_dir = dirname(__FILE__);
        my $awk_script = "$script_dir/create_testdata.awk";

        # Create temporary .lmn file using AWK transformation
        my $temp_lmn = "$output_il.lmn";

        system("awk -f '$awk_script' '$lmntest_file' > '$temp_lmn'");

        # Compile to .il using LMNtal compiler
        system("java -cp '$lmntal_home/bin/lmntal.jar' runtime.FrontEnd --slimcode '$temp_lmn' > '$output_il'");

        unlink($temp_lmn);
    }

    return $output_il;
}

print $count . ".." . ($#ARGV) . "\n";  # Subtract 1 since first arg is binary path
for my $i (1..$#ARGV) {
	$filename = $ARGV[$i];

	# Generate .il file if needed and get the path to run SLIM on
	my $il_to_run = generate_il_file($filename);

	# Extract .lmntest filename from source .il path to get expected output
	$lmntest_file = $filename;
	$lmntest_file =~ s/\.il$/.lmntest/;

	my $expected_output = "";
	my $expected_type = "ok";                                  # "ok" or "ng" from line 3
	my $computed_result = "";

	if (-f $lmntest_file) {
		open (FILE, "< " . $lmntest_file);
		@file = <FILE>;
		$expected_output = $file[1];                           # Line 2 is expected output
		$expected_output =~ s/\n//;                            # Remove newline
		$expected_type = $file[2] // "ok";
		$expected_type =~ s/\s//g;                             # Strip whitespace/newline
		close (FILE);
	} else {
		$expected_output = "expected output";
	}

	# For verbose mode, also run the original computation to see actual result
	if ($ENV{VERBOSE} || $ENV{V}) {
		my $build_dir = $ENV{TEST_BUILD_DIR};
		my $orig_il_file;
		if ($build_dir) {
			my $suite = basename(dirname($filename));
			my $case_base = basename($filename, '.il');
			$orig_il_file = "$build_dir/generated/$suite/${case_base}_orig.il";
			make_path("$build_dir/generated/$suite") unless -d "$build_dir/generated/$suite";
		} else {
			$orig_il_file = $filename;
			$orig_il_file =~ s/\.il$/_orig.il/;
		}

		if (-f $lmntest_file) {
			# Extract first line (original LMNtal program) from .lmntest file
			open(my $test_fh, '<', $lmntest_file) or die "Cannot read $lmntest_file: $!";
			my $lmntal_program = <$test_fh>;
			close($test_fh);
			chomp($lmntal_program);

			# Create temporary .lmn file with just the LMNtal program
			my $temp_lmn = "$orig_il_file.lmn";

			open(my $lmn_fh, '>', $temp_lmn) or die "Cannot create $temp_lmn: $!";
			print $lmn_fh $lmntal_program . "\n";
			close($lmn_fh);

			# Compile to .il using LMNtal compiler
			system("java -cp '$lmntal_home/bin/lmntal.jar' runtime.FrontEnd --slimcode '$temp_lmn' > '$orig_il_file'");

			# Run original computation to get actual result
			my $orig_run = $lmntal_runtime . " --hide-ruleset " . $options ." ". $orig_il_file;
			$computed_result = `$orig_run 2>&1`;
			$computed_result =~ s/\n//;
			$computed_result =~ s/\s*\.\s*$//;  # Remove trailing dot and whitespace

			# Clean up temporary files
			unlink($temp_lmn);
			unlink($orig_il_file);
		}
	}

	# Run the equivalence check (create_testdata.awk version)
	$check_run = $lmntal_runtime . " --hide-ruleset " . $options ." ". $il_to_run;
	$checked = `$check_run 2>&1`;
	my $exit_code = $? >> 8;

	# Clean up the actual output for display
	my $display_output = $checked;
	$display_output =~ s/\n//;

	# Check for segmentation fault or other runtime crashes
	if ($exit_code != 0 || ($checked eq "" && $expected_output ne "")) {
		print "not ok " . $count;
		if ($exit_code != 0) {
			print " - SLIM crashed with exit code $exit_code";
		} else {
			print " - SLIM produced no output (possible segfault)";
		}
		print "\n";
		$test_failed = 1;
		$count = $count + 1;
		next;
	}

	# Check outcome against expected type (ok/ng from line 3 of .lmntest):
	# - ok test: passes if SLIM outputs "ok" (computation matched expected)
	# - ng test: passes if SLIM does NOT output "ng" (computation correctly didn't match the bad expected)
	my $slim_said_ok = (index($checked, "ok") == 0);
	my $slim_said_ng = (index($checked, "ng") == 0);
	my $test_passed  = ($expected_type eq "ng") ? !$slim_said_ng : $slim_said_ok;

	if ($test_passed) {
		print "ok " . $count;
		# Verbose logging: show what was computed vs expected even for passing tests
		if ($ENV{VERBOSE} || $ENV{V}) {
			if ($expected_type eq "ng") {
				print " - correctly did not produce '$expected_output'";
			} else {
				print " - computed '$computed_result' matches expected '$expected_output'";
			}
		}
	} else {
		print "not ok " . $count;
		if ($expected_type eq "ng") {
			print " - result should NOT be '$expected_output'";
		} else {
			print " - '" . $display_output . "' should be '" . $expected_output . "'";
		}
		$test_failed = 1;
	}
	print "\n";
	$count = $count + 1;
}

exit($test_failed ? 1 : 0);
