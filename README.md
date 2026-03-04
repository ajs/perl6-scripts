# perl6-scripts

A staging ground for coding exercises and experiments in the
[Raku](https://raku.org/) (formerly Perl 6) language, along with related
Perl utility modules.

## Scripts

### `non-trivial-powers.pl`

A Raku script that finds all *non-trivial powers* — positive integers that can
be expressed as **a^b** in more than one way — up to **2^20**.  It was inspired
by a [Python blog post](http://blogs.fluidinfo.com/terry/2011/03/30/the-eighty-six-non-trivial-powers-220/)
and prints each such number alongside every pair `(a, b)` that produces it.

**Example output (excerpt):**

```
     64 = 2^6, 4^3, 8^2
    512 = 2^9, 8^3
   4096 = 2^12, 4^6, 8^4, 16^3
```

### `p5.p6`

A Raku script written as an entry for the
[P5: Find the Longest Common Substring](http://strangelyconsistent.org/blog/p5-find-the-longest-common-substring)
coding challenge hosted by *masak* on the Strangely Consistent blog.

The script implements and benchmarks several independent solutions to the
Longest Common Substring (LCS) problem, including two original approaches
(`ajs-list` and `ajs-substr`) as well as reference implementations contributed
by other participants in the challenge (`p5-colomon`, `p5-matthias`,
`p5-moritz`, `p5-fox`, `p5-mberends`, and `p5-util`).

The key insight behind the `ajs-substr` approach is to process at most one
character comparison per step, maintaining a running list of *active* matches.
This yields near-linear performance on typical English-language strings but
degrades on inputs with a highly restricted token set (e.g. genetic sequences),
where bookkeeping overhead dominates.

## Modules

### `Unicode-Represent.pm`

A Perl 5 module (`Unicode::Represent`) that converts a Unicode string into a
chosen target encoding as *succinctly as possible*, without losing information.
Unlike a plain `Encode::encode` call, it falls back gracefully for characters
that cannot be represented in the target encoding, inserting human-readable
escape sequences instead of dying or silently mangling the text.

**Quick example:**

```perl
use Unicode::Represent;

print represent(
    "ascii",
    "u\x{0308}\x{0160}\x{FB04}\x{E6}",
    unicode_display => "full");
# Input characters: ü (u + U+0308), Š (U+0160), ﬄ (U+FB04), æ (U+00E6)
# => u(U+0308: COMBINING DIAERESIS)S(U+030C: COMBINING CARON)ffl(U+00E6: LATIN SMALL LETTER AE)
# Note: U+0160 (LATIN CAPITAL LETTER S WITH CARON) is decomposed into
#       'S' (ASCII-representable base letter) + U+030C (combining diacritic).
```

Fallback representations are configurable via the `unicode_display` option:
`perl` (default, `\x{0000}`), `unicode` (`(U+0000)`), `python` (`\u0000`),
`name` (`(NULL)`), and `full` (`(U+0000: NULL)`).

See the embedded POD documentation (`perldoc Unicode-Represent.pm`) for the
full API reference.

## Author

Aaron Sherman &lt;ajs@ajs.com&gt;
