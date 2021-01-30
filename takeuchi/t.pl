#!/usr/bin/perl

use strict;
use warnings;
use v5.10;

# Turn off the "deep recursion" warnings.
no warnings "recursion";

my %cache;

sub lookup_or_eval {
	my $result;

	my $tmp = $cache{$_[0]};

	if (defined $tmp) {
		$result = $tmp;
	} else {
		$result = eval $_[0];
		$cache{$_[0]} = $result;
	}

	return $result;
}

sub t {
	my $x = lookup_or_eval($_[0]);
	my $y = lookup_or_eval($_[1]);

	if ($x <= $y) {
		return $y;
	}

	my $z = lookup_or_eval($_[2]);

	my $nx = $x - 1;
	my $ny = $y - 1;
	my $nz = $z - 1;
	@_ = ("t('return $nx', 'return $y', 'return $z')",
	      "t('return $ny', 'return $z', 'return $x')",
	      "t('return $nz', 'return $x', 'return $y')");

        # Use manual tail recursion to avoid some "deep recursion" warnings.
      	goto &t;
}

if ($#ARGV != 2) {
	die "You need to specify three integers.";
}

my $result = t("return $ARGV[0]", "return $ARGV[1]", "return $ARGV[2]");

say $result;
