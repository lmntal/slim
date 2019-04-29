#!/usr/bin/perl
######################################
#                                    #
# Java LMNtal system checker on Perl #
#                                    #
######################################
# AUTHOR : kudo, saito
# LAST UPDATE : 2019/04/29

# This is LMNtal system checker.

use Cwd;
$pwd = Cwd::getcwd();

$lmntal_runtime = $pwd . "/../../src/slim";
$count = 1;

print $count . ".." . ($#ARGV + 1) . "\n";
for (@ARGV) {
	$filename = $_;
	$flags_option = "";
	if (exists($ENV{SLIM_FLAG})) {
		$flags_option = $ENV{SLIM_FLAG};
	}
	$check_run = $lmntal_runtime . " " . $flags_option . " " . $pwd . $filename . ".il";
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
