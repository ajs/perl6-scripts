# Represent a given Unicode string in a target encoding, as succinctly
# as possible, without throwing away information. As long as the given
# input string is valid Unicode, the output will be a valid target
# encoding sequence.

package Unicode::Represent;

use POSIX qw(ceil);
use Unicode::Normalize qw(NFC NFD NFKD);
use Encode;
use Carp qw(confess);
use List::Util qw(max);

use feature qw(switch);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(represent);

use strict;
use warnings;

# Convert a string to a target encoding via Encode::encode,
# setting $@ on failure.
sub try_convert {
    my ($encoding, $s, $fail) = @_;
    $@ = undef;
    my $empty = "";
    encode($encoding, $empty, Encode::FB_CROAK); # Test $encoding first
    if ($fail) {
	return encode($encoding, $s, Encode::FB_CROAK);
    } else {
	return eval { encode($encoding, $s, Encode::FB_CROAK) };
    }
}

# Format codepoint as "(U+XXXX)"
sub display_unicode {
    my($char, $min_digits) = @_;
    $min_digits = max($min_digits, 4);
    return display_hex(['(U+', ')'], $char, $min_digits);
}

# Format codepoint as "NAME"
sub display_unicode_name_bare {
    my($char) = @_;
    use charnames ();
    return charnames::viacode($char);
}

# Format codepoint as "(NAME)"
sub display_unicode_name {
    my($char, $min_digits) = @_;
    return '(' . display_unicode_name_bare($char) . ')'
}

# Format codepoint as "(U:XXXX: NAME)"
sub display_unicode_full {
    my($char, $min_digits) = @_;
    my $hex = display_hex(['U+'], $char, $min_digits);
    my $bare = display_unicode_name_bare($char);
    return "($hex: $bare)";
}

# Format codepoint as "\uXXXX"
sub display_python {
    my($char, $min_digits) = @_;
    if ($min_digits < 4) {
	die "python Unicode display mode requires 4 digit minimum";
    }
    return display_hex(['\u'], $char, $min_digits);
}

# Format codepoint as "\x{XXXX}"
sub display_perl {
    my($char, $min_digits) = @_;
    return display_hex(['\x{', '}'], $char, $min_digits);
}

# Hex digits for Unicode codepoint with framing and/or zero-padding
sub display_hex {
    my($framing, $char, $min_digits) = @_;
    my $n = max($min_digits, ceil(length(sprintf("%x", $char)) / 2)*2);
    my($prefix, $suffix) = @$framing;
    $suffix //= "";
    return sprintf("%s%0${n}X%s", $prefix, $char, $suffix);
}

# The primary interface for this module
sub represent {
    my ($encoding, $s, %options) = @_;

    $encoding or die "Must provide a target encoding for represent()";
    $s // return(undef);
    my $min_digits = $options{min_digits} // 4;
    if ($min_digits % 2 != 0) {
	die "min_digits must be a multiple of 2";
    }
    my $compatibility = $options{compatibility} // 1;
    my $decomp = $compatibility ? \&NFKD : \&NFD;
    my $unicode_display;
    given ($options{unicode_display}) {
	when ('unicode') { $unicode_display = \&display_unicode }
	when ('full') { $unicode_display = \&display_unicode_full }
	when ('python') { $unicode_display = \&display_python }
	when ('name') { $unicode_display = \&display_unicode_name }
	default { $unicode_display = \&display_perl }
    }

    # Strings that convert cleanly require no other work
    my $out = try_convert($encoding, $s);

    if ($@) {
	# Try unicode NFC (Canonical Decomposition + Canonical Composition)
	$s = NFC($s);
	$out = try_convert($encoding, $s);

	if ($@) {
	    $out = try_convert($encoding, "");
	    # Resort to character-by-character conversion
	    for my $cp (unpack("U*", $s)) {
		# This character might be fine...
		my $ucp = try_convert($encoding, pack("U", $cp));
		if (! $@) {
		    $out .= $ucp;
		} else {
		    # Try normalizing by decomposition, then check each
		    # resulting character's conversion status.
		    for my $decomp_cp (unpack("U*",
					      $decomp->(pack("U", $cp)))) {
			$ucp = try_convert($encoding, pack("U", $decomp_cp));
			if (! $@) {
			    $out .= $ucp;
			} else {
			    # Give up and just insert the codepoint as
			    # text.
			    $out .= try_convert(
				$encoding,
				$unicode_display->($decomp_cp, $min_digits),
				'fail');
			}
		    }
		}
	    }
	}
    }
    return $out;
}

1;

__END__

=encoding utf-8

=head1 NAME

Unicode::Represent - Represent Unicode strings in a target encoding

=head1 SYNOPSIS

  use Unicode::Represent;

  my $encoded_sequence = represent("Latin1", $unicode_string);

  represent("ascii", $str, min_digits=>2, compatibility=>0);
  represent("ascii", $str, min_digits=>4, unicode_display=>"full");
  represent("EBCDIC::cp37", $str);

  print represent(
    "ascii",
    "u\x{0308}\x{0160}\x{FB04}\x{E6}",
    unicode_display=>"full");

  u(U+0308: COMBINING DIAERESIS)S(U+030C: COMBINING CARON)ffl(U+00E6: LATIN SMALL LETTER AE)

=head1 DESCRIPTION

Given a valid Unicode string such as B<üŠﬄ> or in Perl,
C<"u\x{0308}\x{0160}\x{FB04}">, this
module provides a single funciton, C<represent>, which returns a
sequence of bytes that represents the input string in a target
encoding. The difference between "representing" a string and "converting"
it is that the represented string aims to provide a human-readable
way of viewing any source string in any encoding.

For example, the above string would convert to Latin1 as C<"üS\x{030C}ffl">
where the escape sequence C<"\x{030C}"> is literally a backslash followed
by C<"x{030C}">, not the Unicode code-point that that represents.

While this output has some extra "noise" in it from the escape sequence,
it maintains as much of the string as possible, given the limitations
of the target encoding.

When converted to "ascii", the output would be C<"u\x{0308}S\x{030C}ffl">,
and notice that in this case, the "u" is preserved and the umlaut is
then indicated by C<"\x{0308}">, a combining code point.

=head1 PARAMETERS:

=over

=item C<$encoding>

The name of the encoding to target. See L<Encode> for valid
values.

=item C<$string>

The Unicode string to convert.

=back

Optional, named parameters:

=over

=item C<min_digits>

Minimum number of digits for escape sequences. Defaults to 4.

=item C<compatibility>

By default, this is true, causing the NFKD funciton to be used as the
final, character-by-character fallback. If set to false, NFD (canonical
decomposition) is used. This is explained in more detail in the
"Unicode Standard Annex #15: UNICODE NORMALIZATION FORMS" document
at unicode.org, but a trivial example is the character C<\x{FB04}>
which is the Unicode ligature that looks like "ffl". NFD will
return the same character, but NFKD will replace the ligature with
the Latin/ASCII characters "ffl".

=item C<unicode_display>

This controls the final fallback for characters that cannot be displayed
in the target encoding. The options are "perl" (the default, C<\x{0000}),
"unicode" (C<(U+0000)>), "python" (C<\u0000>), "name" (C<(NULL)>)
and "full" (C<(U+0000: NULL)>).

=back

=head1 USES

This module is intended for use in representing Unicode strings in
easily hashable ways that maintain human readability and reversability.
This can be useful in storing values into non-Unicode-aware databases,
for converting text into document systems that are not Unicode-aware,
etc.

If you want to be able to reverse the process after, just be sure to
escape any backslashes in the input first:

  $s =~ s|\\|\\\\|g;

=head1 DETAILS

In order to do this, several strategies are employed:

=over

=item *

Attempt to use the Encode (see L<Encode>) module to simply convert the input string
to the target encoding and return it if successful.

=item *

Use Unicode::Normalize's (see L<Unicode::Normalize>) C<NFC> to convert the string to a composed form.

=item *

Again try to convert the whole string and return it if successful.

=item *

Split the composed string up into individual code-points.

=item *

Attempt to convert the single code-point to the target encoding.
(from this point on, any successful conversion is saved to the output
sequence and the next code-point is examined)

=item *

Use Unicode::Normalize's NFKD to convert the single code-point to a
decomposed, compatibility form and loop over each, now-decomposed,
code-point.

=item *

Attempt to convert the decomposed code-point to the target encoding.

=item *

As a final fallback, the decomposed, compatibility code-point is
rendered as a Perl-friendly escape sequence of the form
C<\x{H}> where "H" is the smallest even number of hexidecimal digits
required to represent the code-point, with a minimum size of C<$min_digits>.

=back

=head1 OUTPUT

The output is a Perl string that has the utf-8 bit turned off, and is
thus interpreted as a sequence of 8-bit bytes. See the Encode module
for more details.

=head1 AUTHOR

Written and copyright 2014 by Aaron Sherman <ajs@ajs.com>

=cut
