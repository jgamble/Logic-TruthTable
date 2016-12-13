=head1 NAME

Logic::TruthTable::Base81 - provide Base81 utility functions to Logic::TruthTable

=cut

package Logic::TruthTable::Base81;

use strict;
use warnings;
use 5.010001;

use Carp;
use Exporter;
our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
	all => [ qw(
		to_base81
		from_base81
		terms_to_base81
		terms_from_base81
	) ],
);

our @EXPORT_OK = (
	@{$EXPORT_TAGS{all}},
);

our $VERSION = 1.00;

=head1 DESCRIPTION

This module provides Base81 utilities designed for JSON attributes.

=cut

=head2 FUNCTIONS

=head3 to_base81()

=head3 from_base81()

    @enc = to_base81($width, $dc, [@columnlist]);

Functions to transform the column values into a more compact form.
Taking four base-3 values (0, 1, don't-care) at a time gives 3**4
(81) possible values, each of which can be encoded into a single
character . Since four values are encoded into one character, the length
of the list is one-fourth the length of the original column.

Since a Base81 representation is one-fourth the length of a column list,
it is more suitable for writing JSON files (which may be done by
using the export_json() and import_json() methods of Logic::TruthTable).

Unless you are creating your own file storage format, you will probably
not need these functions for your own code.

    #
    # Prepare a Base81 list for writing to a JSON file later.
    #
    my @b81col = to_base81(6, '-',
        [split(//, "--0111000011001100000000----1-1-101--10000011-1-101--100----1-1-")]
    );

    #
    # Having read a Base81 string from a JSON file, use it to create an object.
    #
    my $tt = Logic::TruthTable->new(
        width => $w,
        dc => '-',
        columns => [
            {
                columnlist => [from_base81($w, '-', \@b81col)],
            }
    );

=head4 Base81

The Base81 character set is adapted from the Base85 character set
described by Robert Elz in his RFC1924 of April 1st 1996,
L<"A Compact Representation of IPv6 Addresses"|https://tools.ietf.org/html/rfc1924>
which are made up from the 94 printable ASCII characters, minus
quote marks, comma, slash and backslash, and the brackets.

Despite it being an
L<April Fool's Day RFC|https://en.wikipedia.org/wiki/April_Fools%27_Day_Request_for_Comments>,
the reasoning for the choice of characters for the set was solid, and
Base81 uses them minus four more characters, the parentheses and the
braces.

This reduces the character set to:

    '0'..'9', 'A'..'Z', 'a'..'z', '!', '#', '$', '%', '&',
    '*', '+', '-', ';', '<', '=', '>', '?', '@', '^', '_',
    '`', '|', and '~'.

=cut

#
# 0..9: 0..9
# A..Z: 10..35
# a..z: 36..61
# punc: 62..80
#
# Or, in more tabular form:
#
#               |    0      1     2      3      4      5      6      7      8
#               +-------------------------------------------------------------
# ('0'..'8')  0 | 0000   0001  000-   0010   0011   001-   00-0   00-1   00--
# ('9'..'H')  9 | 0100   0101  010-   0110   0111   011-   01-0   01-1   01--
# ('I'..'Q') 18 | 0-00   0-01  0-0-   0-10   0-11   0-1-   0--0   0--1   0---
# ('R'..'Z') 27 | 1000   1001  100-   1010   1011   101-   10-0   10-1   10--
# ('a'..'i') 36 | 1100   1101  110-   1110   1111   111-   11-0   11-1   11--
# ('j'..'r') 45 | 1-00   1-01  1-0-   1-10   1-11   1-1-   1--0   1--1   1---
# ('s'..'!') 54 | -000   -001  -00-   -010   -011   -01-   -0-0   -0-1   -0--
# ('#'..'<') 63 | -100   -101  -10-   -110   -111   -11-   -1-0   -1-1   -1--
# ('='..'~') 72 | --00   --01  --0-   --10   --11   --1-   ---0   ---1   ----
#

#
# Take a number from 0 to 80, and turn it into a character.
#
my @b81_encode = ('0' .. '9', 'A' .. 'Z', 'a' .. 'z',
	'!', '#', '$', '%', '&', '*', '+', '-', ';',
	'<', '=', '>', '?', '@', '^', '_', '`', '|', '~');

#
# Take the ord() of a character, and return the number (from 0 to 80)
# for it. Wrong characters return -1.
#
my @b81_decode = (
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 62, -1, 63, 64, 65, 66, -1, -1, -1, 67, 68, -1, 69, -1, -1,
	 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, 70, 71, 72, 73, 74,
	75, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, 76, 77,
	78, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
	51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, 79, -1, 80, -1);

sub to_base81
{
	my($width, $dc, $columnref) = @_;
	my $last_idx = (1 << $width) - 1;

	#
	# Set up the conversion hash and convert the column list
	# into two-bit values. This has to be done on the fly
	# as the don't-care character is changeable.
	#
	my %x3 = (
		'0' => 0b00,	# 0/false
		'1' => 0b01,	# 1/true
		$dc => 0b10,	# don't care
	);

	my @blist = map{$x3{$_}} @{ $columnref };
	my $str;

	if ($width == 1)
	{
		return $b81_encode[27 * $blist[0] + 9 * $blist[1]];
	}

	for my $j (1 .. scalar(@blist) >> 2)
	{
		my($z, $y, $x, $w) = splice(@blist, 0, 4);
		$str .= $b81_encode[27*$z + 9*$y + 3*$x + $w];
	}
	return $str;
}

sub from_base81
{
	my($width, $dc, $base81str) = @_;
	my $last_idx = (1 << $width) - 1;
	my @char81 = split(//, $base81str);
	my @val81 = map{$b81_decode[ord($_)]} @char81;

	#
	# Set up the conversion array on the fly because
	# the don't-care character is changeable.
	#
	my(@c3) = ('0', '1', $dc);
	my $c_idx = 0;
	my @clist;

	for my $x (@val81)
	{
		if ($x == -1)
		{
			carp "Incorrect character '" .
				$char81[$c_idx] . "' at position " .
				$c_idx . "; cannot create columnlist";
			return ();
		}

		push @clist, $c3[int($x/27)];
		push @clist, $c3[int(($x % 27)/9)];
		push @clist, $c3[int(($x % 9)/3)];
		push @clist, $c3[$x % 3];
		$c_idx++;
	}

	#
	# Does the string we read in create a column of the correct length?
	# (With an edge case exception, of course.)
	#
	if ($#clist > $last_idx)
	{
		carp "Too many characters in Base81 string" unless ($width == 1 and $#clist == 0);
		splice(@clist, $last_idx - $#clist);
	}
	elsif ($#clist < $last_idx)
	{
		carp "Base81 string not long enough for width $width";
	}

	return @clist;
}

sub terms_to_base81
{
	my($width, $isminterms, $termref, $dontcaresref)= @_;
	my ($dfltbit, $setbit) = ($isminterms)? (0, 1): (1, 0);
	my @blist = ($dfltbit) x (1 << $width);

	#
	# Set up the list of 0s, 1s, and 2s (for don't-cares)
	# to be sliced into Base81 code.
	#
	map {$blist[$_] = $setbit} @{$termref};
	map {$blist[$_] = 2} (@{ $dontcaresref});

	if ($width == 1)
	{
		return $b81_encode[27 * $blist[0] + 9 * $blist[1]];
	}

	my $str;

	for (1 .. (scalar(@blist) >> 2))
	{
		my($z, $y, $x, $w) = splice(@blist, 0, 4);
		$str .= $b81_encode[27*$z + 9*$y + 3*$x + $w];
	}
	return $str;
}

sub terms_from_base81
{
	my($width, $base81str) = @_;
	my $last_idx = (1 << $width) - 1;
	my @char81 = split(//, $base81str);
	my @val81 = map{$b81_decode[ord($_)]} @char81;

	my(@maxterms, @minterms, @dontcares);
	my $c_idx = 0;

	#
	# Does the string we read in create a column of the correct length?
	# (With an edge case exception, of course.)
	#
	if (length($base81str) == (1 << ($width - 2)) or (length($base81str) == 1 and $width == 1))
	{
		my $t = 0;
		for my $x (@val81)
		{
			if ($x == -1)
			{
				carp "Incorrect character '" .
					$char81[$c_idx] . "' at position " .
					$c_idx . "; cannot create columnlist";
				return ();
			}

			for ( int($x/27), int(($x % 27)/9), int(($x % 9)/3), ($x % 3))
			{
				if ($_ == 1)
				{
					push @minterms, $t;
				}
				elsif ($_ == 0)
				{
					push @maxterms, $t;
				}
				else
				{
					push @dontcares, $t;
				}
				$t++;
			}
			$c_idx++;
		}
	}

	return (\@minterms, \@maxterms, \@dontcares);
}


=head1 SEE ALSO

L<Logic::TruthTable::Util>

=head1 AUTHOR

John M. Gamble C<< <jgamble@cpan.org> >>

=cut

1;

__END__

