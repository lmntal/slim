#!/usr/bin/perl
##########################################
# IL checker for SLIM (Unified)          #
# - Runs .iltest and .iltest.nd          #
# - .iltest format:                      #
#     <IL code...>                       #
#     %%EXPECT%%                         #
#     <expected stdout...>               #
# - .iltest.nd format:                   #
#     <IL code...>                       #
#     %%EXPECT%%                         #
#     (stored)   X                       #
#     (end)      Y                       #
# - Prints TAP format                    #
##########################################

use strict;
use warnings;
use Cwd;
use File::Temp qw(tempfile);

my $pwd = Cwd::getcwd();
my $slim = $ENV{SLIM_BINARY} || "$pwd/../../build/bin/slim";
my $options = $ENV{slim_CHECK_OPTIONS} || "";

my $total = scalar(@ARGV);
print "1..$total\n";

my $count = 1;
for my $target (@ARGV) {
  my ($test_path, $is_nd) = resolve_test_path($pwd, $target);

  if (!defined $test_path || !-f $test_path) {
    my $shown = defined($test_path) ? $test_path : $target;
    print "not ok $count - missing test file: $shown\n";
    $count++;
    next;
  }

  my ($il_src, $expected, $parse_err) = parse_iltest($test_path);
  if (defined $parse_err) {
    print "not ok $count - $parse_err\n";
    $count++;
    next;
  }

  my ($fh, $tmp_il) = tempfile("slim_ilcheck_XXXX", SUFFIX => ".il", UNLINK => 0);
  print {$fh} $il_src;
  close($fh);

  my $cmd;
  if ($is_nd) {
    my $nd_flags = "--nd --hl --use-Ncore=4";
    # Omit --hide-ruleset for ND runs as it wasn't in the original check.pl
    $cmd = build_command($slim, "$nd_flags $options", $tmp_il, 0);
  } else {
    $cmd = build_command($slim, $options, $tmp_il, 1);
  }

  my $actual = `$cmd`;
  my $exit_code = $? >> 8;

  unlink($tmp_il) if -f $tmp_il;

  if ($is_nd) {
    my ($actual_stored) = ($actual =~ /\(stored\)\s+=\s+(\d+)/);
    my ($actual_ends)   = ($actual =~ /\(end\)\s+=\s+(\d+)/);

    my ($expected_stored, $expected_ends) = ($expected =~ /(\d+)/g);

    $actual_stored   = defined($actual_stored)   ? $actual_stored   : "undef";
    $actual_ends     = defined($actual_ends)     ? $actual_ends     : "undef";
    $expected_stored = defined($expected_stored) ? $expected_stored : "undef";
    $expected_ends   = defined($expected_ends)   ? $expected_ends   : "undef";

    if ($actual_stored eq $expected_stored && $actual_ends eq $expected_ends) {
      print "ok $count\n";
    } else {
      print "not ok $count - stored = $actual_stored (expected $expected_stored), end = $actual_ends (expected $expected_ends)\n";
    }
  } else {
    if ($exit_code != 0) {
      chomp($actual);
      print "not ok $count - slim exited with code $exit_code";
      print " ('$actual')" if $actual ne "";
      print "\n";
      $count++;
      next;
    }

    normalize_newlines(\$actual);
    normalize_newlines(\$expected);
    canonicalize_for_compare(\$actual);
    canonicalize_for_compare(\$expected);

    if ($actual eq $expected) {
      print "ok $count\n";
    } else {
      my $msg = first_diff_message($actual, $expected);
      print "not ok $count - output mismatch: $msg\n";
    }
  }

  $count++;
}

sub resolve_test_path {
  my ($cwd, $target) = @_;

  my $t = $target;
  $t =~ s/^\s+|\s+$//g;

  my $is_nd = ($t =~ s/\.iltest\.nd$//) || ($t =~ /\.nd$/);
  $t =~ s/\.iltest$//;

  # Normalize leading slash (treat as repo-local path, not filesystem absolute)
  $t =~ s{^/}{};

  if ($cwd =~ m{(?:^|/)testsuite$} && $t =~ m{^testsuite/}) {
    $t =~ s{^testsuite/}{};
  }

  if ($is_nd || -f "$cwd/$t.iltest.nd") {
    return ("$cwd/$t.iltest.nd", 1);
  }
  return ("$cwd/$t.iltest", 0);
}

sub parse_iltest {
  my ($path) = @_;
  my $content = read_all($path);

  my $sep = "%%EXPECT%%";
  my $idx = index($content, $sep);
  if ($idx < 0) {
    return ("", "", "invalid test format: missing %%EXPECT%% in $path");
  }

  my $il_src   = substr($content, 0, $idx);
  my $expected = substr($content, $idx + length($sep));

  $expected =~ s/^\r?\n//;

  return ($il_src, $expected, undef);
}

sub read_all {
  my ($path) = @_;
  open(my $fh, "<", $path) or return "";
  local $/ = undef;
  my $s = <$fh>;
  close($fh);
  return defined($s) ? $s : "";
}

sub normalize_newlines {
  my ($ref) = @_;
  $$ref =~ s/\r\n/\n/g;
}

sub canonicalize_for_compare {
  my ($ref) = @_;

  $$ref =~ s/\r\n/\n/g;
  $$ref =~ s/[ \t]+$//mg;
  $$ref =~ s/\.(\n?)$/$1/;
}

sub shell_quote {
  my ($s) = @_;
  $s =~ s/'/'"'"'/g;
  return "'$s'";
}

sub build_command {
  my ($slim_bin, $opts, $il_path, $hide_ruleset) = @_;

  my $cmd = shell_quote($slim_bin);
  $cmd .= " --hide-ruleset" if $hide_ruleset;
  if (defined $opts && $opts ne "") {
    $cmd .= " $opts";
  }
  $cmd .= " " . shell_quote($il_path);
  return $cmd;
}

sub first_diff_message {
  my ($a, $b) = @_;

  my @aa = split(/\n/, $a, -1);
  my @bb = split(/\n/, $b, -1);
  my $max = @aa > @bb ? scalar(@aa) : scalar(@bb);

  for (my $i = 0; $i < $max; $i++) {
    my $av = $i < @aa ? $aa[$i] : "<EOF>";
    my $bv = $i < @bb ? $bb[$i] : "<EOF>";
    if ($av ne $bv) {
      my $line = $i + 1;
      return "line $line expected '$bv' but got '$av'";
    }
  }

  return "unknown difference";
}
