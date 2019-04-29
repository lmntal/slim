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

print $count . ".." . (($#ARGV + 1) / 3) . "\n";
for (my $i = 0; $i <= $#ARGV; $i += 3) {
	$filename = $ARGV[$i];
	$flags_fixed = "--nd --hl";
	$flags_option = "--use-Ncore=4";
	if (exists($ENV{SLIM_FLAG})) {
		$flags_option = $ENV{SLIM_FLAG};
	}
	$check_run = $lmntal_runtime . " " . $flags_fixed . " " . $flags_option . " " . $filename . ".il";
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
