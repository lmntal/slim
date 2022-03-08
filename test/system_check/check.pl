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
$pwd = Cwd::getcwd();

$lmntal_runtime = $pwd . "/../../src/slim";
$count = 1;
$options = $ENV{slim_CHECK_OPTIONS};

print $count . ".." . ($#ARGV + 1) . "\n";
for (@ARGV) {
	$filename = $_;
	$check_run = $lmntal_runtime . " " . $options ." ". $pwd . $filename . ".il";
	$checked = `$check_run`;

	$result = index ($checked, "ok");

	if ($result == 0) {
		print "ok " . $count;
	} else {
		open (FILE, "< " . $pwd . $filename . ".lmntest");
		@file = <FILE>;
		$file[1] =~ s/\n//;                                # delete return code
		$checked =~ s/\n//;
		print "not ok " . $count;
		print " - '" . $checked . "' should be '" . $file[1] . "'";
		close (FILE);
	}
	print "\n";
	$count = $count + 1;
}
