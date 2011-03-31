#!../rakudo/perl6

# Based on a python posting here:
# http://blogs.fluidinfo.com/terry/2011/03/30/the-eighty-six-non-trivial-powers-220/

my %powers;
my $lim = 20;

for 2 ... $lim -> $a {
  for 2 ... $lim -> $b {
     my $n = $a ** $b;
     last if $n > 2**$lim;
     (%powers{+$n} //= []).push([$a,$b]);
  }
}

for %powers.sort: +*.key {
  say .key.fmt("%7d") ~ " = " ~ .value>>.join("^").join(", ");
}
