package Logic::TruthTable;

use 5.010001;
use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;
use namespace::autoclean;

use Carp;
use List::MoreUtils qw(pairwise);
use Module::Runtime qw(is_module_name use_module);
use Text::CSV;

use Logic::Minimizer;

#
# TBD: Parallelize the column-solving. Some
# recommended modules below, choose later.
#
#use Parallel::ForkManager;
#use MCE;
#use Smart::Comments ('###'); 

#
# Base class of the minimizer (used by
# Algorithm::QuineMcCluskey).
#
class_type 'ColumnMinimizer',
	{class => 'Logic::Minimizer'};

#
# Define the array of minimizer/hashref types
# used to define our table's function columns.
#
subtype 'ArrayRefOfColumnMinimizers',
	as 'ArrayRef[ColumnMinimizer]';

subtype 'ArrayRefOfHashRef',
	as 'ArrayRef[HashRef]';

#
# The width attribute is fed into the
# minimizer object; it cannot be overridden
# by the minimizer's attributes.
#
has 'width' => (
	isa => 'Int', is => 'ro', required => 1
);

#
# The don't-care character and the vars attributes on the
# other hand, are merely defaults and *can* be overridden
# by the object.
#
has 'dc' => (
	isa => 'Str', is => 'rw',
	default => '-'
);

has 'vars' => (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	default => sub{['A' .. 'Z'];},
);

#
# Used to determine which minimizer object type will be
# created by default for the columns. As of this release,
# only Algorithm::QuineMcCluskey is available.
#
has 'algorithm' => (
	isa => 'Str', is => 'ro', required => 0,
	default => 'QuineMcCluskey',
);

#
# The function names for each column.
#
has 'functions' => (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	default => sub{['F0' .. 'F9', 'F10' .. 'F31'];},
);

#
# The column objects. Either the array ref of hash refs (i.e., the plain
# text), or the algorithm object.
#
has 'columns' => (
	isa => 'ArrayRefOfHashRef|ArrayRefOfColumnMinimizers',
	is => 'ro', required => 1,
	reader => '_get_columns',
	writer => '_set_columns',
	predicate => 'has_columns'
);

#
# The title of the truth table.
#
has 'title' => (
	isa => 'Str', is => 'rw', required => 0,
	predicate => 'has_title'
);

#
# Number of columns (functions). Stored so that we don't have to
# go nuts with array sizing an array reference in an object.
#
has '_fn_width' => (
	isa => 'Int', is => 'rw', required => 0
);

#
# Hash look-up by name instead of by index for column (function)
# or var column.
#
has ['_fn_lookup', '_var_lookup'] => (
	isa => 'HashRef', is => 'rw', required => 0,
);

=head1 NAME

Logic::TruthTable - Create and solve sets of boolean equations.

=head1 VERSION

Version 1.00

=cut

our $VERSION = '1.00';


=head1 SYNOPSIS

Create a truth table.

 
    #
    # Create a "Rock-Paper-Scissors winners" truth table, using
    # the following values:
    #
    # Columns represent (in two bits) the winner of Rock (01)
    # vs. Paper (10), or vs. Scissors (11). A tie is 00.
    #
    # 
    #        a1 a0 b1 b0 ||  w1 w0
    #       -----------------------
    #  0     0  0  0  0  ||  -  -
    #  1     0  0  0  1  ||  -  -
    #  2     0  0  1  0  ||  -  -
    #  3     0  0  1  1  ||  -  -
    #  4     0  1  0  0  ||  -  -
    #  5     0  1  0  1  ||  0  0    (tie)
    #  6     0  1  1  0  ||  1  0    (paper)
    #  7     0  1  1  1  ||  0  1    (rock)
    #  8     1  0  0  0  ||  -  -
    #  9     1  0  0  1  ||  1  0    (paper)
    # 10     1  0  1  0  ||  0  0    (tie)
    # 11     1  0  1  1  ||  1  1    (scissors)
    # 12     1  1  0  0  ||  -  -
    # 13     1  1  0  1  ||  0  1    (rock)
    # 14     1  1  1  0  ||  1  1    (scissors)
    # 15     1  1  1  1  ||  0  0    (tie)
    #

    use Logic::TruthTable;

    my $ttbl = Logic::TruthTable->new(
        width => 4,
        title => 'Rock Paper Scissors Winner Results',
        vars => ['a1', 'a0', 'b1', 'b0'],
        functions => ['w1', 'w0'],
        columns => [
            {
                minterms => [6, 9, 11, 14],
                dontcares => [0 .. 4, 8, 12],
            },
            {
                minterms => [7, 11, 13, 14],
                dontcares => [0 .. 4, 8, 12],
            },
        ],
    );

    #
    # Print the result
    #
    print $ttbl->fnsolve();

    #
    # Save the truth table values as a CSV file.
    #
    open my $fh, ">", "rpswinners.csv" or croak "Error opening CSV file.";
    $ttbl->export(write_handle => \$fh);
    close $fh;


=head1 Description

This module minimizes tables of 
L<Boolean expressions|https://en.wikipedia.org/wiki/Boolean_algebra> using the
algorithms available on CPAN.

=head2 Object Methods

=head3 new()

Create the truth table object. The attributes are:

=over 4

=item 'width'

The number of variables (input columns) in the Boolean expressions.

This is a required attribute.

=item 'title'

A title for the problem you are solving.

=item 'dc'

I<Default value: '-'>

Change the representation of the don't-care character. The don't-care
character is used both in the columnstring, and internally as a place
holder for eliminated variables in the equation. Some of those internals
may be examined via other methods.

This becomes the I<default> value of the function columns; it may be
individually overridden in each C<columns> attribute.

=item 'vars'

I<Default value: ['A' .. 'Z']>

The variable names used to form the equation. The names will be taken from
the leftmost first.

This becomes the I<default> value of the function columns; it may be
individually overridden in each C<columns> attribute.

=item 'functions'

I<Default value: ['F0' .. 'F9', 'F10' .. 'F31']>

The function names used to form the equation.

=item 'algorithm'

The default algorithm that will be used to minimize each column.

Currently, as there is only one minimizer algorithm (L<Algorith-QuineMcCluskey>)
available on CPAN, it is the default.

The name will come from the package name, e.g., having an attribute
C<algorithm => 'QuineMcCluskey'> means that the column will be minimized
using the package Algorithm::QuineMcCluskey.

The algorithm module must be installed, and must be of the form
C<Algorithm::Name>. The module must also have Logic::Minimizer as its
parent class. This ensures that it will have the methods needed by
Logic::TruthTable to create and solve the Boolean expressions.

This becomes the I<default> value of the function columns; it may be
individually overridden in each C<columns> attribute.

=item 'columns'

An array of hash references that create the algorithm minimizer objects.

Each column becomes an object itself, and must have the attributes necessary
to create the object.

    #
    # Create a truth table for converting zero to nine (binary)
    # to a 2-4-2-1 code.
    #
    my $tt_2421 = Logic::TruthTable->new(
        width => 4,
        algorithm => 'QuineMcCluskey',
        title	=> "A four-bit binary to 2-4-2-1 converter",
        vars => ['w' .. 'z'],
        functions => [qw(a3 a2 a1 a0)],
        columns => [
            {
                minterms => [ 5 .. 9 ],
                dontcares => [ 10 .. 15 ],
            },
            {
                minterms => [ 4, 6 .. 9 ],
                dontcares => [ 10 .. 15 ],
            },
            {
                minterms => [ 2, 3, 5, 8, 9 ],
                dontcares => [ 10 .. 15 ],
            },
            {
                minterms => [ 1, 3, 5, 7, 9 ],
                dontcares => [ 10 .. 15 ],
            },
        ],
    );

Alternatively, it is possible to pre-create the algorithm minimizer objects,
and use them directly in the C<columns> array, although it does result in
a lot of duplicated code:

    my $q3 = Algorithm::QuineMcCluskey->new(
        title	=> "Column 3 of a four bit binary to 2-4-2-1 converter",
        width => 4,
        minterms => [ 5 .. 9 ],
        dontcares => [ 10 .. 15 ],
        vars => ['w' .. 'z'],
    );
    my $q2 = Algorithm::QuineMcCluskey->new(
        title	=> "Column 2 of a four bit binary to 2-4-2-1 converter",
        width => 4,
        minterms => [ 4, 6 .. 9 ],
        dontcares => [ 10 .. 15 ],
        vars => ['w' .. 'z'],
    );
    my $q1 = Algorithm::QuineMcCluskey->new(
        title	=> "Column 1 of a four bit binary to 2-4-2-1 converter",
        width => 4,
        minterms => [ 2, 3, 5, 8, 9 ],
        dontcares => [ 10 .. 15 ],
        vars => ['w' .. 'z'],
    );
    my $q0 = Algorithm::QuineMcCluskey->new(
        title	=> "Column 0 of a four bit binary to 2-4-2-1 converter",
        width => 4,
        minterms => [ 1, 3, 5, 7, 9 ],
        dontcares => [ 10 .. 15 ],
        vars => ['w' .. 'z'],
    );

    #
    # Create the truth table using the above
    # Algorithm::QuineMcCluskey objects.
    #
    my $tt_2421 = Logic::TruthTable->new(
        width => 4,
        title	=> "A four-bit binary to 2-4-2-1 converter",
        vars => ['w' .. 'z'],
        functions => [qw(a3 a2 a1 a0)],
        columns => [$q3, $q2, $q1, $q0],
    );

=back

=cut

sub BUILD
{
	my $self = shift;
	my $w = $self->width;
	my @cols = @{$self->_get_columns};
	my @fn_names = @{$self->functions};
	my @vars = @{$self->vars};
	my $dc = $self->dc;

	#
	# Make sure the number of function names and variables
	# get set correctly.
	#
	croak "Not enough function names for your columns" if ($#fn_names < $#cols);

	$#fn_names = $#cols;
	$self->functions(\@fn_names);
	$self->_fn_width($#cols);

	$#vars = $w - 1;
	$self->vars(\@vars);
	$self->title("$w-variable truth table in $#cols columns") unless ($self->has_title);

	#
	# Set up the look-up-by-name hashes.
	#
	$self->_fn_lookup({ map{ $fn_names[$_], $_} (0 .. $#fn_names) });
	$self->_var_lookup({ map{ $vars[$_], $_} (0 .. $#vars) });

	#
	# Set up the individual columns, using defaults
	# from the truth table object, if present.
	#
	for my $idx (0 .. $#cols)
	{
		my %tcol = %{ $cols[$idx] };
		$tcol{width} //= $w;
		$tcol{dc} //= $dc;
		$tcol{algorithm} //= $self->algorithm;
		$tcol{vars} //= [@vars];
		$tcol{title} //= $fn_names[$idx];

		croak "Column $idx: width => " . $tcol{width} .
			" doesn't match table's width $w" if ($tcol{width} != $w);

		${$self->_get_columns}[$idx] = new_minimizer_obj(\%tcol);
	}

	return $self;
}

#
# new_minimizer_obj(%algorithm_options)
#
# Creates a column's object (like an Algorithm::QuineMcCluskey object,
# for example) from the options provided.
#
sub new_minimizer_obj
{
	my($href) = @_;
	my %args = %{$href};
	my $al;

	#
	# Find out which object we're creating.
	#
	($al = $args{algorithm}) =~ s/-/::/;
	$al = "Algorithm::" . $al;

	croak "Invalid module name '$al'" unless (is_module_name($al));

	my $obj = use_module($al)->new(%args);
	croak "Couldn't create '$al' object" unless defined $obj;

	return $obj;
}

=head3 get_fncolumn()

Return a column object by name or index.

The columns of a C<Logic::TruthTable> object are themselves
objects, of types C<Algorithm::Name>, where I<Name> is the
algorithm, and which may be set using the C<algorithm> parameter
in C<new()>. (As of this writing, the only algorithm availble
in the CPAN ecosystem is C<Algorithm::QuineMcCluseky>.)

Each column is named via the C<functions> attribute in C<new()>, and
a column can be retrieved using its name.

    my $ttable = Logic::TruthTable->new(
        title => "An Example",
	width => 5,
	functions => ['F1', 'F0'],
        algorithm => 'QuineMcCluskey',  # Our only choice currently.
        columns => [
            {
                minterms => [6, 9, 23, 27],
                dontcares => [0, 2, 4, 16, 24],
            },
            {
                minterms => [7, 11, 19, 23, 29, 30],
                dontcares => [0, 2, 4, 16, 24],
            },
        ],
    );

    my $col_f0 = $ttable->get_fncolumn('F0');

C<$col_f0> will be an Algorithm::QuineMcCluskey object with minterms
(7, 11, 19, 23, 29, 30). As columns are numbered in array order, the
same column could have been retrieved with:

    my $col_f0 = $ttable->get_fncolumn(1);


=cut

sub get_fncolumn
{
	my $self = shift;
	my($fn_name) = @_;
	my $idx;

	#
	#### Let's look at the key: $fn_name
	#### Let's look at the hash: %{$self->_fn_lookup()}
	#### Let's look an an element: $self->_fn_lookup()->{$fn_name}
	#

	$idx = ($self->_fn_lookup()->{$fn_name}) // $fn_name;

	return undef unless ($idx =~ m/^\d+$/ and $idx <= $self->_fn_width);
	return $self->_get_columns()->[$idx];
}

=head3 solve()

=cut

sub solve
{
	my $self = shift;
	my @solns;

	for my $col (@{$self->_get_columns})
	{
		#
		##### Solving: $col
		#
		my $eqn = $col->solve();
		push @solns, $eqn;
	}

	#
	#### solve returns: @solns
	#
	return @solns;
	#return map {$_->solve()} @{$self->_get_columns};
}

=head3 fnsolve()

=cut

sub fnsolve
{
	my $self = shift;
	my(@f) = @{ $self->functions() };
	my(@eqns) = $self->solve();

	return pairwise {qq($a = $b)} @f, @eqns;
}

=head3 export_csv()

Write the truth table out to a CSV file, suitable for reading by
other programs, such as a spreadsheet application, or by
L<Logic Friday|http://sontrak.com/>, a tool for working with logic
functions.

In the example below, a file is being written out for reading
by Logic Friday. Note that Logic Friday insists on its own
don't-care character:

    if (open my $fh, ">:encoding(utf8)", "ttmwc.csv")
    {
        #
        # Override the don't-care character, as Logic Friday
        # insists on it being an 'X'.
        #
        $truthtable->export_csv(write_handle => $fh, dc => 'X');

        close $fh;
    }

The options are:

=over 2

=item write_handle

The opened file handle for writing.

=item dc

The don't-care symbol to use in the file.

=back

The method returns undef if an error is encountered. On
success it returns itself.

Note that this method cannot write Logic Friday's minimized export
format, only the full, not-minimized files.

=cut

sub export_csv()
{
	my $self = shift;
	my(%opts) = @_;

	my $handle = $opts{write_handle};

	### handle: $handle

	unless (defined $handle)
	{
		carp "export_csv(): no file opened for export.";
		return undef;
	}

	my $w = $self->width;
	my $dc = $opts{dc} // $self->dc;
	my $fmt = "%0${w}b";
	my $lastrow = (1 << $w) - 1;
	my @columns;

	#
	# Set up the array of column strings.
	#
	# Set up a potential regex in case the don't-care
	# character is different than the columns' dc.
	#
	### dc: $dc
	#
	for my $c_idx (0 .. $self->_fn_width)
	{
		my $obj = ${$self->_get_columns}[$c_idx];
		my @c = @{$obj->to_columnlist};

		if ($dc ne $obj->dc)
		{
			my $obj_dc = $obj->dc;
			### obj_dc: $obj_dc
			### to dc: $dc
			$_ =~ s/\Q$obj_dc\E/$dc/ for (@c);
		}

		push @columns, [@c];
	}

	#
	# Open the CSV file, print out the header, then each row.
	#
	my $csv = Text::CSV->new( {binary => 1, eol => "\012"} );

	unless ($csv)
	{
		carp "Cannot use Text::CSV: " . Text::CSV->error_diag();
		return undef;
	}

	$csv->print($handle, [@{$self->vars}, '', @{$self->functions}]);

	for my $r_idx (0 .. $lastrow)
	{
		my @row = (split(//, sprintf($fmt, $r_idx)), '');

		push @row, shift @{ $columns[$_] } for (0 .. $self->_fn_width);

		$csv->print($handle, [@row]);
	}

	return $self;
}

=head3 import_csv()

Read a previously written CSV file and create a Logic::TruthTable
object from it.

    #
    # Read in a CSV file.
    #
    if (open my $lf, "<:encoding(utf8)", "excess_3.csv")
    {
        $truthtable = Logic::TruthTable->import_csv(
            read_handle => $lf,
        );
        close $lf;
    }


The don't-care character doesn't have to be set in this case, as import_csv()
will assume anything not a 0 or 1 is the don't-care character. Of course you
may set it anyway if you want to change it from the default don't-care character
in the truth table object.

You can set whether the truth table object is created using the
minterms or the maxterms of the CSV file by using the C<termtype>
attribute:

    $truthtable = Logic::TruthTable->import_csv(
        read_handle => $lf,
        termtype => 'maxterms',        # or 'minterms'.
    );

By default the object is created with minterms.

In addition to the termtype, you may also set the title, don't-care character,
and algorithm attributes. Width, variable names, and function names cannot be
set as these are read from the file.

    $truthtable = Logic::TruthTable->import_csv(
        read_handle => $lf,
        title => "Excess-3 multiplier",
        dc => '.',
        algorithm => 'QuineMcCluskey'
    );

The method returns undef if an error is encountered.

Note that this method cannot read Logic Friday's minimized export
format, only the full, not-minimized files.

=cut

sub import_csv
{
	my $self = shift;
	my(%opts) = @_;

	my $handle = $opts{read_handle};
	my $termtype = $opts{termtype} // 'minterms';

	my @vars;
	my @functions;
	my $width = 0;

	unless (defined $handle)
	{
		carp "import_csv(): no file opened.";
		return undef;
	}
	unless ($termtype =~ /minterms|maxterms/)
	{
		carp "Incorrect value for termtype ('minterms' or 'maxterms')";
		return undef;
	}

	my $csv = Text::CSV->new( {binary => 1} );

	unless ($csv)
	{
		carp "Cannot use Text::CSV: " . Text::CSV->error_diag();
		return undef;
	}

	#
	# Parse the first line of the file, which is the header,
	# and which will have the variable and function names, which
	# in turn will let us deduce the width.
	#
	my $header = $csv->getline($handle);

	#
	### The header is: $header
	#
	for (@$header)
	{
		#
		### Examining: $_
		#
		if ($_ eq '')
		{
			if ($width != 0)
			{
				carp "File is not in the correct format";
				return undef;
			}

			$width = scalar @vars;
		}
		elsif ($width == 0)
		{
			push @vars, $_;
		}
		else
		{
			push @functions, $_;
		}
	}

	#
	# Now that we've got our width, var names, and
	# function names, collect the terms.
	#
	### width: $width
	### termtype: $termtype
	### functions: @functions
	### vars: @vars
	#
	my($termrefs, $dcrefs);

	my $idx = 0;
	while (my $row = $csv->getline($handle))
	{
		for my $c (0 .. $#functions)
		{
			my $field = 1 + $c + $width;

			if ($row->[$field] !~ /[01]/)
			{
				push @{ $dcrefs->[$c] }, $idx;
			}
			elsif (($termtype eq 'minterms' and $row->[$field] eq '1') or
				($termtype eq 'maxterms' and $row->[$field] eq '0'))
			{
				push @{ $termrefs->[$c] }, $idx;
			}
		}
		$idx++;
	}

	#
	# We've collected our variable names, function names, and terms.
	# Let's make an object.
	#
	### dcrefs: $dcrefs
	### termrefs: $termrefs
	#
	my $title = $opts{title} // "$width-input table created from import file";
	my $algorithm = $opts{algorithm} // 'QuineMcCluskey';
	my $dc = $opts{dc} // '-';
	my @columns;

	for my $c (0 .. $#functions)
	{
		push @columns, {
			dontcares => $dcrefs->[$c],
			$termtype, $termrefs->[$c]
		};
	}

	return Logic::TruthTable->new(
		width => $width,
		title => $title,
		dc => $dc,
		vars => [@vars],
		functions => [@functions],
		columns => [@columns],
		algorithm => $algorithm,
	);
}


=head1 AUTHOR

John M. Gamble, C<< <jgamble at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-boolean-truthtable at rt.cpan.org>,
or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Logic-TruthTable>. I will
be notified, and then you'll automatically be notified of progress on your
bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Logic::TruthTable

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Logic-TruthTable>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Logic-TruthTable>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Logic-TruthTable>

=item * Search CPAN

L<http://search.cpan.org/dist/Logic-TruthTable/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2015 John M. Gamble.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See L<http://dev.perl.org/licenses/> for more information.


=cut

1; # End of Logic::TruthTable
