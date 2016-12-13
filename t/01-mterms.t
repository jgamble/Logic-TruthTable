#!perl -T
use 5.010001;
use strict;
use warnings;
use Test::More tests => 4;

use Logic::TruthTable::Util qw(:all);

my $width = 4;
my(@terms, @col0, @col1);

#
# Test the minterms.
#
push_minterm_columns($_, $_ + 3, \@col1, \@col0) for (0 .. 7);
is_deeply(\@col1, [0, 3, 4, 7],
	"push_minterm_columns() col1: [" . join(",", @col1) . "]");

is_deeply(\@col0, [0, 2, 4, 6],
	"push_minterm_columns() col0: [" . join(",", @col0) . "]");

#
# Reset for maxterms.
#
@col0 = ();
@col1 = ();

push_maxterm_columns($_, $_ + 3, \@col1, \@col0) for (0 .. 7);
is_deeply(\@col1, [1, 2, 5, 6],
	"push_maxterm_columns() col1: [" . join(",", @col1) . "]");

is_deeply(\@col0, [1, 3, 5, 7],
	"push_maxterm_columns() col0: [" . join(",", @col0) . "]");


