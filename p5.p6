#!./perl6

# Based on the contest described at:
# http://strangelyconsistent.org/blog/p5-find-the-longest-common-substring
# I came up with a simple approach. My goal was to never deal with
# more that one character comparison per step. That is,
# I never want to do more than compare the $i'th character in string 1
# to some other character in string 2 or compare the $i'th character in
# string 2 to some other character in string 1. At which point, I make
# a note in my ever-growing list of active matches and move on.
#
# What this produces is a solution which has nearly linear performance
# across English strings of up to 400 or so characters, beating
# the competition rather soundly on some of these longer cases.
#
# However, for cases where the input strings have high numbers of
# short overlaps (e.g. with a restricted token set like a genetic
# sequence), we spend all of our time bookkeeping the outstanding
# matches, and our performance craters.
#
# It's an interesting trade-off, and one that I think is worth keeping
# in mind, should you have need of common substring identification.
#
# Things that might speed this up:
# * Start off by splitting the input strings on characters that don't
#   exist in their counterpart. e.g. "abxcde" vs. "bacd" would immediately
#   devolve to these searches: ("ba" vs. "acd", "cd" vs. "acd").
# * Look at the token set in the source strings and, if it's highly
#   restricted, use a suffix-tree approach.
# * Do some look-ahead to short-circut cases where only a single
#   character matches and discard early.

# There are two solutions, below. The first breaks up the strings into
# lists of characters and then starts work. The second works directly
# on the base strings using substr, and appears to be faster in
# all cases, as a result.

# Example pairs of strings that we'll run trials on.
my @samples = (
   [
      # http://strangelyconsistent.org/blog/p5-find-the-longest-common-substring
      "otherworldlinesses",
      "physiotherapy"
   ], [
      # http://justrakudoit.wordpress.com/2011/03/09/more-on-masaks-p5/
      "There were nobles, who made war against each other; there was the king,",
      "who made war against the cardinal; there was Spain, which made war against the king."
   ], [
      # http://justrakudoit.wordpress.com/2011/03/09/more-on-masaks-p5/
      "In those times panics were common, and few days passed without some city or other registering in its archives an event of this kind.",
      "There were nobles, who made war against each other; there was the king, who made war against the cardinal; there was Spain, which made war against the king."
   ], [
      # http://justrakudoit.wordpress.com/2011/03/09/more-on-masaks-p5/
      "In those times panics were common, and few days passed without some city or other registering in its archives an event of this kind. There were nobles, who made war against each other; there was the king, who made war against the cardinal; there was Spain, which made war against the king.",
      "Then, in addition to these concealed or public, secret or open wars, there were robbers, mendicants, Huguenots, wolves, and scoundrels, who made war upon everybody. The citizens always took up arms readily against thieves, wolves or scoundrels, often against nobles or Huguenots, sometimes against the king, but never against cardinal or Spain. It resulted, then, from this habit that on the said first Monday of April, 1625, the citizens, on hearing the clamor, and seeing neither the red-and-yellow standard nor the livery of the Duc de Richelieu, rushed toward the hostel of the Jolly Miller. When arrived there, the cause of the hubbub was apparent to all.",
   ], [
      # http://justrakudoit.wordpress.com/2011/03/09/more-on-masaks-p5/
      "taaatatattgtttcatagtcgtaccacccaacacattgtcctcacg",
      "cataaccggcggctcgtggctttctgtagaccgaatcttcgctgtttgctctg"
   ], [
      # http://justrakudoit.wordpress.com/2011/03/09/more-on-masaks-p5/
      "accgccaaccggaaagaatgtcctcccaccacaaatgtacgctcgcatggcggttgtcgagtctatgtcggttgcgctatactacatgataatggacgccttacttacccaaagtagaaataagctcgtctttgagaaccgtggactggtactacctatttttagtcaaactcatgactcgcgcctagcccacatacaat",
      "tttgtccctggccacgacgttctactatatgttaatgaaacgtaaggaattgcgttggccaagaaacgtccttttcacagatacccgtcgtacctgattaccgctgtagggcgctttttccggctggggcgcgcgtgtctgttggccgggccctacgtaggcctataacggaaagatttgtaccaaattctactacgagg"
   ], [
      # And now, some randomly-generated inputs that are constructed
      # during the test.
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(50).join(''),
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(50).join(''),
   ], [
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(100).join(''),
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(100).join(''),
   ], [
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(200).join(''),
      "abcdefghijklmnopqrstuvwxyz1234567890".comb.roll(200).join(''),
   ], [
      # A worst-case scenario for this solver.
      "abc".comb.roll(50).join,
      "abc".comb.roll(50).join
   ]
);

my %solutions = (
   'ajs-list' => &findlongest,
   'ajs-substr' => &findlongest_substr,
   'p5-util' => &util-lcs,
   'p5-matthias' => &matthias-longest,
   'p5-moritz' => &moritz-LCS,
   'p5-fox' => &fox-longest-string,
   'p5-colomon' => &colomon-find-longest-substring
);

my @lineup = <ajs-substr p5-colomon>;

my $trial = 1;
for @samples -> $s {
    say "";
    print "Searching for common substrings between: ";
    say "'{shortstr($s[0],15)}' and '{shortstr($s[1],15)}'";
    my $total_len = $s[0].chars + $s[1].chars;
    say "Total string length $total_len";
    for @lineup -> $solution-name {
	my $solution = %solutions{$solution-name};
	say "Trying $solution-name...";
	my $start = now;
	say "Answer is: '{$solution($s[0], $s[1])}'";
	say "Solution took {timeinfo($start, now, $total_len)}";
    }
}

sub findlongest($s1, $s2) {
   my @s1 = $s1.comb;
   my @s2 = $s2.comb;
   my @matches; # Completed substring matches
   my @active; # Substring matches we're still adding to
   my @where; # Previously seen possitions of chars in $s1 ([0]) and $s2 ([1])
   my $maxlen = +@s1 > +@s2 ?? +@s1 !! +@s2;
   my $longest_seen = 0;
   for 0 ...^ $maxlen -> $i {
      my @nowactive;
      if +@s1 - $i > $longest_seen {
         @active.push({ 'start1' => $i, 'start2' => $i, 'len' => 0 });
      }
      for @active -> $a {
         # This much space must be left in the strings:
         my $speclen = $a<len> > $longest_seen ?? $a<len> !! $longest_seen;
         if $a<start1>+$a<len> == $i and
            $a<start1>+$speclen < +@s1 and
            $a<start2>+$speclen < +@s2 and
            @s1[$i] eq @s2[$a<start2>+$a<len>] {
            # This active match has continued at @s1[$i]
            $a<len>++;
            @nowactive.push($a);
         } elsif $a<start2>+$a<len> == $i and
            $a<start2>+$speclen < +@s2 and
            $a<start1>+$speclen < +@s1 and
            @s2[$i] eq @s1[$a<start1>+$a<len>] {
            # This active match has continued at @s2[$i]
            $a<len>++;
            @nowactive.push($a);
         } elsif $a<len> > $longest_seen {
            # An active match has ended and is our new champ
            #say " ... adding match '{substr($s1,$a<start1>,$a<len>)}'";
            $longest_seen = $a<len>;
            @matches.push($a);
         }
      }
      if $i < +@s1 and @where[1]{@s1[$i]} {
         for @(@where[1]{@s1[$i]}) -> $j {
            # Add a new active match for the char at @s1[$i] against
            # a previous instance of that char in @s2
            @nowactive.push({ 'start1' => $i, 'start2' => $j, 'len' => 1 });
         }
      }
      if $i < +@s2 and @where[0]{@s2[$i]} {
         for @(@where[0]{@s2[$i]}) -> $j {
            # Add a new active match for the char at @s2[$i] against
            # a previous instance of that char in @s1
            @nowactive.push({ 'start1' => $j, 'start2' => $i, 'len' => 1 });
         }
      }
      # We've built a new list of active matches, so swap.
      @active = @nowactive;
      if $i < +@s1 {
         # Record that we saw this char at @s1[$i]
         @where[0]{@s1[$i]} //= [];
         @where[0]{@s1[$i]}.push($i);
      }
      if $i < +@s2 {
         # Record that we saw this char at @s2[$i]
         @where[1]{@s2[$i]} //= [];
         @where[1]{@s2[$i]}.push($i);
      }
   }
   #say " ... done with main loop.";
   for @active -> $a {
      if $a<len> > $longest_seen {
         $longest_seen = $a<len>;
         #say " ... adding match '{substr($s1,$a<start1>,$a<len>)}'";
         @matches.push($a);
      }
   }
   return "" if $longest_seen == 0;
   # We've actually gathered all of the common substrings,
   # which is kind of interesting on its own, but just return
   # one of the longest (if multiple longest substrings
   # are found, we don't weight one over the other, just pick
   # whichever shows up a the top of the list.
   my $longest = (sort {- .<len>}, @matches)[0];
   return substr($s1, $longest<start1>, $longest<len>);
}

# Same as above, but use substr instead of breaking the
# strings up into arrays. This tends to be around 10% faster.
sub findlongest_substr($s1, $s2) {
   my @matches; # Completed substring matches
   my @active; # Substring matches we're still adding to
   my @where; # Previously seen possitions of chars in $s1 ([0]) and $s2 ([1])
   # Save the lengths of the 2 strings to avoid extra method calls
   my $s1c = $s1.chars;
   my $s2c = $s2.chars;
   my $maxlen = $s1c > $s2c ?? $s1c !! $s2c;
   my $longest_seen = 0;
   for 0 ...^ $maxlen -> $i {
      # Save the substr lookups for the $i'th char of string $s1 and $s2
      my $s1i = $i < $s1c ?? substr($s1,$i,1) !! '';
      my $s2i = $i < $s2c ?? substr($s2,$i,1) !! '';
      my @nowactive;
      if $s1c - $i > $longest_seen {
         @active.push({ 'start1' => $i, 'start2' => $i, 'len' => 0 });
      }
      for @active -> $a {
         # This much space must be left in the strings:
         my $speclen = $a<len> > $longest_seen ?? $a<len> !! $longest_seen;
         if $a<start1>+$a<len> == $i and
            $a<start1>+$speclen < $s1c and
            $a<start2>+$a<len> < $s2c and
            $s1i eq substr($s2,$a<start2>+$a<len>,1) {
            # This active match has continued at $s1[$i]
            $a<len>++;
            @nowactive.push($a);
         } elsif $a<start2>+$a<len> == $i and
            $a<start2>+$speclen < $s2c and
            $a<start1>+$a<len> < $s1c and
            $s2i eq substr($s1,$a<start1>+$a<len>,1) {
            # This active match has continued at $s2[$i]
            $a<len>++;
            @nowactive.push($a);
         } elsif $a<len> > $longest_seen {
            # An active match has ended
            $longest_seen = $a<len>;
            @matches.push($a);
         }
      }
      if $i < $s1c and @where[1]{$s1i} {
         for @(@where[1]{$s1i}) -> $j {
            # Add a new active match for the char at $s1[$i] against
            # a previous instance of that char in $s2
            @nowactive.push({ 'start1' => $i, 'start2' => $j, 'len' => 1 });
         }
      }
      if $i < $s2c and @where[0]{$s2i} {
         for @(@where[0]{$s2i}) -> $j {
            # Add a new active match for the char at $s2[$i] against
            # a previous instance of that char in $s1
            @nowactive.push({ 'start1' => $j, 'start2' => $i, 'len' => 1 });
         }
      }
      say "We now have {+@nowactive} active matches";
      # We've built a new list of active matches, so swap.
      @active = @nowactive;
      if $i < $s1c {
         # Record that we saw this char at $s1[$i]
         @where[0]{$s1i} //= [];
         @where[0]{$s1i}.push($i);
      }
      if $i < $s2c {
         # Record that we saw this char at $s2[$i]
         @where[1]{$s2i} //= [];
         @where[1]{$s2i}.push($i);
      }
   }
   for @active -> $a {
      if $a<len> > $longest_seen {
         $longest_seen = $a<len>;
         @matches.push($a);
      }
   }
   return "" if $longest_seen == 0;
   # We've actually gathered all of the common substrings,
   # which is kind of interesting on its own, but just return
   # one of the longest (if multiple longest substrings
   # are found, we don't weight one over the other, just pick
   # whichever shows up a the top of the list.
   my $longest = (sort {- .<len>}, @matches)[0];
   return substr($s1, $longest<start1>, $longest<len>);
}

# Return $s unless $s > $n chars long. In which case, return a subset of
# $s concatenated with $extra, such that the total is $n chars long exactly.
sub shortstr($s, $n, $extra='...') {
   return $s if $s.chars <= $n;
   return substr($s, 0, $n-$extra.chars) ~ $extra;
}

# Return timing information for a test that started at $start,
# ended at $end and search a total, concatenated string length of $len
sub timeinfo($start, $end, $len) {
   my $duration = $end - $start;
   my $sec_per_char = $duration/$len;
   return sprintf("%.3fsec (%.4fsec/char)", $duration, $sec_per_char);
}

######## Other solutions:

class SuffixTree {
    has $.string;
    has %.edges;
    has @.nodes;
    has $.node-count is rw = 1;

    class Suffix {
        has SuffixTree $.tree;
        has Int $.origin_node is rw;
        has Int $.first_char_index is rw;
        has Int $.last_char_index is rw;

        method new(SuffixTree $tree, Int $node, Int $start, Int $stop) {
            self.bless(*, :$tree, :origin_node($node), :first_char_index($start), :last_char_index($stop));
        }

        method is-explicit { $.first_char_index > $.last_char_index; }
        method is-implicit { !self.is-explicit; }
        method canonize() {
            if !self.is-explicit {
                my $edge = $.tree.edges{$.tree.hash-tag($.origin_node, $.first_char_index)};
                my $edge_span = $edge.last_char_index - $edge.first_char_index;
                while $edge_span <= $.last_char_index - $.first_char_index {
                    $.first_char_index = $.first_char_index + $edge_span + 1;
                    $.origin_node = $edge.end_node;
                    if ($.first_char_index <= $.last_char_index) {
                        $edge = $.tree.edges{$.tree.hash-tag($edge.end_node, $.first_char_index)};
                        $edge_span = $edge.last_char_index - $edge.first_char_index;
                    }
                }
            }
        }
    }

    class Edge {
        has SuffixTree $.tree;
        has Int $.first_char_index is rw;
        has Int $.last_char_index is rw;
        has Int $.start_node is rw;
        has Int $.end_node is rw;

        multi method new(SuffixTree $tree,
                         Int $first_char_index,
                         Int $last_char_index,
                         Int $parent_node) {
            self.bless(*, :$tree, 
                          :$first_char_index, 
                          :$last_char_index, 
                          :start_node($parent_node), 
                          :end_node($tree.node-count++));
        }

        method hash-tag() {
            $.tree.hash-tag($.start_node, $.first_char_index);
        }
    }

    #################################
    ## SuffixTree methods start here!
    #################################

    method new(Str $s) {
        self.bless(*, :string($s));
    }

    method hash-tag($node, $first_char_index) {
        $.string.substr($first_char_index, 1) ~ $node;
    }

    method split-edge(Edge $edge, Suffix $s) {
        %.edges.delete($edge.hash-tag);
        my $new-edge = Edge.new(self,
                                $edge.first_char_index,
                                $edge.first_char_index + $s.last_char_index - $s.first_char_index,
                                $s.origin_node);
        %.edges{$new-edge.hash-tag} = $new-edge;
        @.nodes[$new-edge.end_node] = $s.origin_node;
        $edge.first_char_index += $s.last_char_index - $s.first_char_index + 1;
        $edge.start_node = $new-edge.end_node;
        %.edges{$edge.hash-tag} = $edge;
        $new-edge.end_node;
    }

    method dump-edges($current-n) {
        say " Start  End  Suf  First Last  String";
        for %.edges.values -> $s {
            my $top;
            if ($current-n > $s.last_char_index ) {
                $top = $s.last_char_index;
            } else {
                $top = $current-n;
            }

            say $s.start_node ~ " "
                ~ $s.end_node ~ " "
                ~ @.nodes[$s.end_node] ~ " "
                ~ $s.first_char_index ~ " "
                ~ $s.last_char_index ~ "     "
                ~ $.string.substr($s.first_char_index, $top - $s.first_char_index + 1);
        }
    }

    method add-prefix(Suffix $active, $last_char_index) {
        my $parent_node;
        my $last_parent_node = -1;

        loop {
            my $edge;
            $parent_node = $active.origin_node;

            # // Step 1 is to try and find a matching edge for the given node.
            # // If a matching edge exists, we are done adding edges, so we break
            # // out of this big loop.

            if $active.is-explicit {
                $edge = %.edges{self.hash-tag($active.origin_node, $last_char_index)};
                last if $edge.defined;
            } else { # implicit node, a little more complicated
                $edge = %.edges{self.hash-tag($active.origin_node, $active.first_char_index)};
                my $span = $active.last_char_index - $active.first_char_index;
                last if $.string.substr($edge.first_char_index + $span + 1, 1)
                        eq $.string.substr($last_char_index, 1);
                $parent_node = self.split-edge($edge, $active);
            }

            # // We didn't find a matching edge, so we create a new one, add
            # // it to the tree at the parent node position, and insert it
            # // into the hash table.  When we create a new node, it also
            # // means we need to create a suffix link to the new node from
            # // the last node we visited.

            my $new-edge = Edge.new(self, $last_char_index, $.string.chars, $parent_node);
            %.edges{$new-edge.hash-tag} = $new-edge;
            if $last_parent_node > 0 {
                @.nodes[$last_parent_node] = $parent_node;
            }
            $last_parent_node = $parent_node;

            # // This final step is where we move to the next smaller suffix

            if $active.origin_node == 0 {
                $active.first_char_index++;
            } else {
                $active.origin_node = @.nodes[$active.origin_node];
            }
            $active.canonize;
        }

        if $last_parent_node > 0 {
            @.nodes[$last_parent_node] = $parent_node;
        }
        $active.last_char_index++;  # Now the endpoint is the next active point
        $active.canonize();
    };

    method make-tree() {
        # // The active point is the first non-leaf suffix in the
        # // tree.  We start by setting this to be the empty string
        # // at node 0.  The AddPrefix() function will update this
        # // value after every new prefix is added.

        my $active = Suffix.new(self, 0, 0, -1);  # The initial active prefix
        for ^($.string.chars) -> $i {
            self.add-prefix($active, $i);
        }
    }

    has %.node-map;

    method update-node-map() {
        %!node-map = ();
        for %.edges.values -> $edge {
            %!node-map.push($edge.start_node => $edge);
        }
    }

    method get-edge-map-for-node($start-node) {
        my %edge-map;
        if %!node-map{$start-node} {
            for @(%!node-map{$start-node}) -> $edge {
                %edge-map.push($.string.substr($edge.first_char_index, 1) => $edge);
            }
        }
        %edge-map;
    }

    my @good-suffixes;
    my @branch-count;
    my $error-found;

    method validate(:$verbose = Bool::False)
    {
        say "\nValidating:" if $verbose;

        self.update-node-map;
        @good-suffixes = Bool::False xx $.string.chars;
        @branch-count = 0 xx self.node-count;
        $error-found = Bool::False;

        self.walk-tree(0, "");

        my $error = 0;
        for @good-suffixes.kv -> $i, $status {
            if !$status {
                say "Suffix $i count wrong!";
                $error++;
                $error-found = Bool::True;
            }
        }
        say "All Suffixes present!" if $error == 0 && $verbose;

        my $leaf_count = 0;
        my $branch_count = 0;
        for ^self.node-count -> $i {
            if @branch-count[$i] == 0 {
                say "Logic error on node $i, not a leaf or internal node!";
                $error-found = Bool::True;
            } elsif @branch-count[$i] == -1 {
                $leaf_count++;
            } else {
                $branch_count += @branch-count[$i];
            }
        }

        if $verbose {
            say "Leaf count: $leaf_count { $leaf_count == $.string.chars ?? " OK" !! " Error!" }";
            say "Branch count: $branch_count { $branch_count == self.node-count - 1 ?? " OK" !! " Error!" }";
        }
        $error-found ||= $leaf_count != $.string.chars || $branch_count != self.node-count - 1;
        !$error-found;
    }

    method walk-tree($start_node, $string-so-far)
    {
        my $edges = 0;
        if %.node-map{$start_node} {
            for @(%.node-map{$start_node}) -> $edge {
                if @branch-count[$edge.start_node] < 0 {
                    say "Logic error on node { $edge.start_node }";
                    $error-found = Bool::True;
                }
                @branch-count[$edge.start_node]++;
                $edges++;
                my $current-string = $string-so-far;
                $current-string ~= $.string.substr($edge.first_char_index, 
                                                   $edge.last_char_index - $edge.first_char_index + 1);
                if $edge.end_node == $start_node {
                    say "Structure error!!!";
                    $error-found = Bool::True;
                }

                if $edge.end_node != $start_node && self.walk-tree($edge.end_node, $current-string) {
                    if @branch-count[$edge.end_node] > 0 {
                        say "Logic error on node { $edge.end_node }";
                        $error-found = Bool::True;
                    }
                    @branch-count[$edge.end_node]--;
                }
            }
        }

        # // If this node didn't have any child edges, it means we
        # // are at a leaf node, and can check on this suffix.  We
        # // check to see if it matches the input string, then tick
        # // off it's entry in the @good-suffixes list.

        if $edges == 0 {
            # say "Suffix: $string-so-far";
            @good-suffixes[$string-so-far.chars - 1]++;
            my $tail-of-string = $.string.substr($.string.chars - $string-so-far.chars);
            if $tail-of-string ne $string-so-far {
                say "Comparison failure!  Expected $tail-of-string but got $string-so-far";
                $error-found = Bool::True;
            }
            Bool::True;
        } else {
            Bool::False;
        }
    }
}

sub colomon-look-for-longest-substring(SuffixTree $st, $length-of-first) 
{
    my @shared-substrings;

    sub walk-tree($start_node, $string-so-far)
    {
        my $edges = 0;
        if $st.node-map{$start_node} {
            my $split = ?@($st.node-map{$start_node}).grep({ $_.first_char_index < $length-of-first })
                     && ?@($st.node-map{$start_node}).grep({ $_.first_char_index > $length-of-first });
            if $split {
                @shared-substrings.push($string-so-far);
            } 

            for @($st.node-map{$start_node}) -> $edge {
                $edges++;
                my $current-string = $string-so-far;
                $current-string ~= $st.string.substr($edge.first_char_index, 
                                                     $edge.last_char_index - $edge.first_char_index + 1);

                walk-tree($edge.end_node, $current-string);
            }
        }
    }

    $st.update-node-map;
    walk-tree(0, "");

    if +@shared-substrings {
        @shared-substrings.max({ $^a.chars <=> $^b.chars });
    } else {
        Any;
    }
}

sub colomon-find-longest-substring($a, $b, :$return-suffixtree = Bool::False) {
    my $term1 = "©";
    my $term2 = "®";
    my $st = SuffixTree.new($a ~ $term1 ~ $b ~ $term2);
    $st.make-tree;
    my $longest = colomon-look-for-longest-substring($st, $a.chars + 1);

    if $return-suffixtree {
        ($st, $longest);
    } else {
        $longest;
    }
}

sub fox-longest-string($str1, $str2) {
    my @str1 = $str1.comb;
    my @str2 = $str2.comb;

    my %table;
    my $length = 0;
    my $result = '';

    for ^@str1 X ^@str2 -> $i, $j {
        if @str1[$i] eq @str2[$j] {
            %table{$i}{$j} = 1 + ( %table{$i-1}{$j-1} // 0);
            if %table{$i}{$j} >= $length {
                $length = %table{$i}{$j};
                $result = [~] @str1[ $i-$length+1 .. $i];
            }
        }
    }
    return $result;
}

sub moritz-LCS($s1, $s2) {
    my $lcs_len = 0;
    my ($l1, $l2) = ($s1, $s2)>>.chars;
    my @char1 = Str, $s1.comb;
    my @char2 = Str, $s2.comb;
    my @previous_suffix = 0 xx ($l2 + 1);
    my $lcs = '';
    for 1..$l2 -> $n1 {
        my @current_suffix = 0 xx ($l2 + 1);
        for 1..$l2 -> $n2 {
            next unless @char1[$n1] eq @char2[$n2];

            # look, current common substring just got one longer!
            # RAKUDO #80614: I would have liked to write that as
            # my $c = @current_suffix[$n] = ...
            # but a bug in rakudo prevents that :(
            @current_suffix[$n2] = my $c = @previous_suffix[$n2 - 1] + 1;


            if $c > $lcs_len {
                # ... and it's the best we've got!
                $lcs_len = $c;
                $lcs = $s1.substr($n1 - $c, $c);
            }
        }
        # avoid copying by using binding
        @previous_suffix := @current_suffix;
    }
    return $lcs;
}

sub matthias-longest($str1, $str2) {
    my @strA = $str1.comb;
    my @strB = $str2.comb;

    my %offsetB;
    %offsetB.push( @strB Z 0..* );

    my $max-substr = '';

    for 0..* Z @strA -> $i, $chr {
        next unless %offsetB.exists: $chr;

        for @( %offsetB{$chr} ) -> $j {
            my $substr = [~] gather for @strA[$i..*-1] Z @strB[$j..*-1] -> $a, $b {
                last if $a ne $b;
                take $a;
            };

            $max-substr = $substr if $substr.chars > $max-substr.chars;
        }

        last if $max-substr.chars >= +@strA | +@strB - $i;
    }

    return  $max-substr;
}

# From http://rosettacode.org/wiki/Longest_common_subsequence#Perl_6
# Be advised: I was not the original author of this code; I adapted and
# refactored it to match the way I would have written it, but it may count
# as plagiarism to you.

sub util-lcs ( Str $xstr, Str $ystr ) {
    my @xs = $xstr.comb;
    my @ys = $ystr.comb;

    my @lengths = map { [ 0 xx (1+@ys) ] }, ^(1+@xs);

    for @xs.keys X @ys.keys -> $i, $j {
        @lengths[$i+1][$j+1] = @xs[$i] eq @ys[$j]
            ??   @lengths[$i][$j]+1
            !! ( @lengths[$i][$j+1], @lengths[$i+1][$j] ).max;
    }

    my ( $x, $y ) = ( +@xs, +@ys );
    my @r = gather while $x and $y {
        my    $len_xy = @lengths[$x][$y];
        if    $len_xy == @lengths[$x-1][$y  ] { $x-- }
        elsif $len_xy == @lengths[$x  ][$y-1] { $y-- }
        else {
            take @xs[$x-1];
            $x--;
            $y--;
        }
    }

    return @r.reverse.join('');
}
