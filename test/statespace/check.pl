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

print $count . ".." . (($#ARGV + 1) / 3) . "\n";
for (my $i = 0; $i <= $#ARGV; $i += 3) {
	$filename = $ARGV[$i];
	$flags = "--nd --hl";
	$check_run = $lmntal_runtime . " " . $flags . " " . $filename . ".il";
	$checked = `$check_run`;

	$checked =~ /\(stored\)\s+=\s+(\d+)/;
	$numofstates = $1;
	$checked =~ /\(end\)\s+=\s+(\d+)/;
	$numofends = $1;

	if ($numofstates == $ARGV[$i+1] && $numofends == $ARGV[$i+2]) {
		print "ok " . $count;
	} else {
		print "not ok " . $count;
		print " - stored = " . $numofstates . ", end = " . $numofends;
	}
	print "\n";
	$count = $count + 1;
}
