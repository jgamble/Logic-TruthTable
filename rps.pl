#!/bin/perl

use strict;
use warnings;

use Logic::TruthTable;
use Logic::TruthTable::Util qw(:all);

my $throws = 5;

my $w = length(sprintf("%b", $throws));

my $two_exp_w = 2 ** $w;

my $length = $two_exp_w - 1;

my $csvfile = "rps" . $length . ".csv";
my $jsonfile = "rps" . $length . ".json";

my @throw = ('Tie', 'T1' .. 'T9', 'T10' .. 'T63')[0 .. $length];

my @vars_a = reverse(('a0' .. 'a8')[0 .. $w-1]);
my @vars_b = reverse(('b0' .. 'b8')[0 .. $w-1]);
my @fns = reverse(('w0' .. 'w8')[0 .. $w-1]);

print join(", ", @vars_a, @vars_b), "\n\n";
my(@dontcares, @w5, @w4, @w3, @w2, @w1, @w0);


my @col_terms;
#for (0 .. 5)
#{
	#my @x;
	#push @col_terms, \@x;
	#push @col_terms, [];
#}

push @col_terms, [] for (0 .. $w-1);

print join(", ", @col_terms), "\n\n";


#
# Now, all the possible combinations of player A vs. player B.
#
for my $j (0 .. $length)
{
	for my $k (0 .. $length)
	{
		my $idx = $j * $two_exp_w + $k;
		if ($k == 0 or $j == 0)
		{
			push @dontcares, $idx;
		}
		else
		{
			my $val = rpswinner($j, $k);
			push_minterm_columns($idx, $val, @col_terms);
		}
	}
}

print "Done creating minterms:\n";

#
# Let's create the columns.
#
my @columns;

for my $idx (0 .. $w-1)
{
	push @columns, {dontcares => [@dontcares], minterms => [@{$col_terms[$idx]}], };
}

my $tt = Logic::TruthTable->new(
	width =>  2 * $w,
	vars => [@vars_a, @vars_b],
	functions => [@fns],
	columns => [@columns],
);

print "Writing JSON file...\n";

open my $jsonfh, ">", $jsonfile or die "oops on $jsonfile";
$tt->export_json(write_handle => $jsonfh);
close $jsonfh;

print "Finished writing JSON file...\n";

print "Writing CSV file...\n";

open my $csvfh, ">", $csvfile or die "oops on $csvfile";
$tt->export_csv(write_handle => $csvfh, dc => 'X');
close $csvfh;

print "Finished writing CSV file...\n";

my @eqns = $tt->solve();
print join("\n\n", @eqns), "\n\n";

@eqns = $tt->fnsolve();
print join("\n\n", @eqns), "\n\n";

@eqns = $tt->all_fnsolutions();
print join("\n\n", @eqns), "\n\n";

exit(0);

sub rpswinner
{
	my($player_a, $player_b) = @_;

	return 0 if ($player_a == $player_b);	# Tie, of course.

	my $val = $player_a - $player_b + $length;

	return $player_a if ($val % 3 == 1);
	return $player_b;
}

