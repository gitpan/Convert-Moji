=head1 NAME

Convert::Moji - convert between lists of characters

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

This module transforms one list of characters into another. Here are
some ways you could make a rot13 transformer with this module:

    use Convert::Moji;
    # Using a table
    my %rot13; @rot13{('a'..'z')} = ('n'..'z','a'..'m');
    my $rot13 = Convert::Moji->new (["table", \%rot13]);
    # Using tr
    my $rot13_1 = Convert::Moji->new (["tr", "a-z", "n-za-m"]);
    # Using a callback
    sub rot_13_sub { tr/a-z/n-za-m/; return $_ }
    my $rot13_2 = Convert::Moji->new (["code", \&rot_13_sub]);

Then to do the actual conversion

    my $out = $rot13->convert ("secret");

and now $out contains "frperg".You also can go backwards with

    my $inverted = $rot13->invert ("frperg");

and now $inverted contains "secret".

=head2 Combining conversions

You can also chain the converters
together, with

    my $does_something = Convert::Moji->new (["table", $mytable],
					     ["tr", $left, $right]);


=head2 Uninvertible operations

If your conversion doesn't actually go backwards, you can tell the
module when you create the object using a keyword "oneway":

    my $uninvertible = Convert::Moji->new (["oneway", "table", $mytable]);

Then $uninvertible->invert doesn't do anything. You can also
selectively choose which operations of a list are invertible and which
aren't, so that only the invertible ones do something.

=head2 Load from a file

You can also load a table from a file using

Convert::Moji->new (["file", $filename]);

In this case, the file needs to contain a space-separated list to be
converted one into the other.

=head3 Bugs

This doesn't handle comments or blank lines in the file.

=cut

package Convert::Moji;

use warnings;
use strict;
use utf8;

use Carp;

# =head2 load_convertor

# Internal routine. Load a convertor from a file.

# =cut

sub load_convertor
{
    my ($file) = @_;
    my $file_in;
    if (! open $file_in, "<:utf8", $file) {
	carp "Could not open '$file' for reading: $!";
	return;
    }
    my %converter;
    while (<$file_in>) {
#	print;
	chomp;
	my ($left, $right) = split /\s+/;
#	print "l = $left r = $right\n";
	$converter{$left} = $right;
    }
    close $file_in or croak "Could not close '$file': $!";
    return \%converter;
}

# length_one

# Internal routine.

# We also need a routine to go from something like

# a: b c d e

# to an ambiguous map.

# Does every element of the array have length one or not?

sub length_one
{
    for (@_) {
	return if !/^.$/;
    }
    return 1;
}

# make_regex

# Internal routine

# Make a regular expression to match any of the characters in the list
# of inputs.

sub make_regex
{
    my @inputs = @_;
    # Quote any special characters. We could also do this with join
    # '\E|\Q', but the regexes then become even longer.
    for (@inputs) { s/([\$\\\/*\.^()+*?{}])/\\$1/g }
    if (length_one (@inputs)) {
	return '(['.(join '', @inputs).'])';
    } else {
	# Sorting is essential, otherwise shorter characters match before
	# longer ones, causing errors if the shorter character is part of
	# a longer one.
	return '('.join ('|',sort { length($b) <=> length($a) } @inputs).')';
    }
}

# _unambiguous

# Internal routine

# True if mapping to/from keys and values is unambiguous both ways.

sub _unambiguous
{
    my ($table) = @_;
    my %inverted;
    for (keys %$table) {
	my $v = $$table{$_};
	return if $inverted{$v};
	$inverted{$v} = $_;
    }
    # Is not ambiguous
    return 1;
}

# If the table is unambiguous, we can use Perl's built-in "reverse"
# function. However, if the table is ambiguous, "reverse" will lose
# information. The method applied here is to make a hash with the
# values of $table as keys and the values are array references.

sub ambiguous_reverse
{
    my ($table) = @_;
    my %inverted;
    for (keys %$table) {
	my $val = $table->{$_};
#	print "Valu is $val\n";
	push @{$inverted{$val}}, $_;
#	print "key $_ stuff ",join (' ',@{$inverted{$val}}),"\n";
    }
    for (keys %inverted) {
	@{$inverted{$_}} = sort @{$inverted{$_}};
    }
    return \%inverted;
}

# Callback

sub split_match
{
    my ($erter, $input, $convert_type) = @_;
    $convert_type = "first" if (!$convert_type);
    my $lhs = $erter->{rhs};
    my $rhs = $erter->{out2in};
    if (!$convert_type || $convert_type eq 'first') {
	$input =~ s/$lhs/$$rhs{$1}->[0]/eg;
	return $input;
    } elsif ($convert_type eq 'random') {
	my $size = @$rhs;
	$input =~ s/$lhs/$$rhs{$1}->[int rand $size]/eg;
	return $input;
    } elsif ($convert_type eq 'all' || $convert_type eq 'all_joined') {
#	print "$lhs $input\n";
	my @output = grep {length($_) > 0} (split /$lhs/, $input);
	for my $o (@output) {
	    if ($o =~ /$lhs/) {
		$o = $$rhs{$1};
	    }
	}
	if ($convert_type eq 'all') {
	    return \@output;
	} else {
	    return join ('',map {ref($_) eq 'ARRAY' ? "[@$_]" : $_} @output);
	}
    } else {
	carp "Unknown convert_type $convert_type";
    }
}

# =head2 table

# internal routine

# Attach a table to a Convert::Moji object.

#=cut

sub table
{
    my ($table, $noinvert) = @_;
    my $erter = {};
    $erter->{type} = "table";
    $erter->{in2out} = $table;
    my @keys = keys %$table;
    my @values = values %$table;
    $erter->{lhs} = make_regex @keys;
    if (!$noinvert) {
	$erter->{unambiguous} = _unambiguous($table);
	if ($erter->{unambiguous}) {
	    my %out2in_table = reverse %{$table};
	    $erter->{out2in} = \%out2in_table;
	} else {
	    $erter->{out2in} = ambiguous_reverse ($table);
	    @values = keys %{$erter->{out2in}};
	}
	$erter->{rhs} = make_regex @values;
    }
#    print "RHS is ",$erter->{rhs},"\n";
    return $erter;
}

# tr_erter

# Internal routine.

# Make a converter from a tr instruction.

sub tr_erter
{
    my ($lhs, $rhs) = @_;
    my $erter = {};
    $erter->{type} = "tr";
    $erter->{lhs} = $lhs;
    $erter->{rhs} = $rhs;
    return $erter;
}

# Add a code-based converter

sub code
{
    my ($convert, $invert) = @_;
    my $erter = {};
    $erter->{type} = "code";
    $erter->{convert} = $convert;
    $erter->{invert} = $invert;
    return $erter;
}

=head2 new

Create the object. Arguments are a list of array references. The array
references should have either the "noninvertible" flag "oneway" or one
of the following as its first argument.

=over

=item table

After this comes one more argument, a reference to the hash containing
the table. For example

     my $conv = Convert::Moji->new (["table", \%crazyhash]);

The hash keys and values can be any length, so you can convert single
characters into words, as in 

     my %crazyhash = {"a" => "apple", "b" => "banana"}

and vice-versa if you wish. The conversion will be performed correctly
regardless of the weirdness of your table.

=item file

After this comes one more argument, the name of a file containing some
information to convert into a hash table. The file format is
space-separated pairs, no comments or blank lines allowed. If the file
does not exist or cannot be opened, the module prints an error
message, and returns the undefined value.

=item code

After this comes one or two references to subroutines. The first
subroutine is the conversion and the second one is the inversion
routine. If you omit the second routine, it is equivalent to
specifying "oneway".

=item tr

After this come two arguments, the left and right hand sides of a "tr"
expression, for example

     Convert::Moji->new (["tr", "A-Z", "a-z"])

will convert upper to lower case

A "tr" is performed, and inversely for the invert case.

=back

Conversions, via "convert", will be performed in the order of the
arguments. Inversions will be performed in reverse order of the
arguments, skipping uninvertibles.

=cut

sub new
{
    my ($package, @conversions) = @_;
    my $conv = {};
    bless $conv;
    $conv->{erter} = [];
    $conv->{erters} = 0;
    for my $c (@conversions) {
	my $noinvert;
	my $erter;
	if ($c->[0] eq "oneway") {
	    shift @$c;
	    $noinvert = 1;
	}
	if ($c->[0] eq "table") {
#	    print "Adding a table\n";
	    $erter = table ($c->[1], $noinvert);
	} elsif ($c->[0] eq "file") {
	    my $file = $c->[1];
#	    print "Adding a table from file '$file'\n";
	    my $table = Convert::Moji::load_convertor ($file);
	    return if !$table;
	    $erter = table ($table, $noinvert);
	} elsif ($c->[0] eq 'tr') {
	    $erter = tr_erter ($c->[1], $c->[2]);
	} elsif ($c->[0] eq 'code') {
	    $erter = code ($c->[1], $c->[2]);
	    if (!$c->[2]) {
		$noinvert = 1;
	    }
	}
	my $o = $conv->{erters};
	$conv->{erter}->[$o] = $erter;
	$conv->{noinvert}->[$o] = $noinvert;
	$conv->{erters}++;
    }
    return $conv;
}

=head2 convert

The convert method takes one argument, which is a scalar string to be
converted into the other list by the stuff we fed in at "new".

=head3 Bugs

=over

=item no "strict conversion"

Just ignores (passes through) characters which it can't convert. It
should have a "strict" option to also validate the input.

=back

=cut

sub convert
{
    my ($conv, $input) = @_;
    for (my $i = 0; $i < $conv->{erters}; $i++) {
#	print "\$i = $i\n";
	my $erter = $conv->{erter}->[$i];
	if ($erter->{type} eq "table") {
	    my $lhs = $erter->{lhs};
	    my $rhs = $erter->{in2out};
	    $input =~ s/$lhs/$$rhs{$1}/g;
	} elsif ($erter->{type} eq 'tr') {
	    my $lhs = $erter->{lhs};
	    my $rhs = $erter->{rhs};
	    eval ("\$input =~ tr/$lhs/$rhs/");
	} elsif ($erter->{type} eq 'code') {
	    $_ = $input;
	    $input = &{$erter->{convert}};
	}
    }
    return $input;
}

=head2 invert

Inverts the input.

Takes two arguments, the first is the string to be inverted back
through the conversion process, and the second is the type of conversion to perform if the inversion is ambiguous. This can take one of the following values

=over

=item first

If the inversion is ambiguous, it picks the first one it finds.

=item random

If the inversion is ambiguous, it picks one at random.

=item all

In this case you get an array reference back containing either strings
where the inversion was unambiguous, or array references to arrays
containing all possible strings. So it's a horrible mess.

=item all_joined

Like "all" but you get a scalar with all the options in square
brackets instead of lots of array references.

=back

=head3 Bugs

=over

=item second argument not implemented fully

The second argument part is only implemented for hash table based
conversions, and is very likely to be buggy even then.

=back

=cut

sub invert
{
    my ($conv, $input, $convert_type) = @_;
    for (my $i = $conv->{erters} - 1; $i >= 0; $i--) {
	next if $conv->{noinvert}->[$i];
	my $erter = $conv->{erter}->[$i];
	if ($erter->{type} eq "table") {
	    if ($erter->{unambiguous}) {
		my $lhs = $erter->{rhs};
		my $rhs = $erter->{out2in};
		$input =~ s/$lhs/$$rhs{$1}/g;
	    } else {
		$input = split_match ($erter, $input, $convert_type);
	    }
	} elsif ($erter->{type} eq 'tr') {
	    my $lhs = $erter->{rhs};
	    my $rhs = $erter->{lhs};
	    eval ("\$input =~ tr/$lhs/$rhs/");
	} elsif ($erter->{type} eq 'code') {
	    $_ = $input;
	    $input = &{$erter->{invert}};
	}
    }
    return $input;
}

1;

__END__

=head1 WHY?

This is a back-end module for other modules which do character
transformations. I hope to release those modules on CPAN soon, so
hopefully the motivation for this will become clearer then.

It was originally a sub-module of another one for converting Japanese
characters ("moji", hence the weird name). During the process of
making this module, I discovered that I also wanted its features for
another module which had absolutely nothing to do with Japanese
characters, so I pulled it out of its submodule status and I'm
releasing it on CPAN as a separate module prior to releasing the other
two. Perhaps it will be useful for someone else as well.

=head1 BUGS

Please report any bugs or feature requests to C<bug-convert-moji at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Convert-Moji>.  I
will be notified, and then you'll automatically be notified of
progress on your bug as I make changes.

=head1 AUTHOR

Ben Bullock, C<< <benkasminbullock at gmail.com> >>

=head1 COPYRIGHT & LICENSE

Copyright 2008 Ben Kasmin Bullock, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut
