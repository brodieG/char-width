# R Character Width Table Update

## Overview

This repository is a collection of scripts, data, and documentation used to
update the tables in 'src/main/rlocale_data.h' from the c.a. 2015 Unicode 8
tables to more recent versions (12.1 under the current proposal).  The
Unicode EastAsianWidth property that is often used to determine terminal cursor
advance of characters has changed significantly since Unicode 8, particularly
for emoji.

The rlocale_data tables are used by `R_nchar` (itself used by `base::nchar`),
and via `R_i18n_wcwidth|R_i18n_wcswidth` in "character.c", "errors.c",
"printUtils.c", "../gnuwin32/console.c" "../gnuwin32/getline/getline.c",
"grDevices/src/{devPS,devPicTex.c}".

These updates are only partial.  Other things that could/should be done:

* Update the other tables in 'rlocale_data.h'.
* Compute width on graphemes instead of Unicode code points; this is likely to
  be a bigger change that could possibly leverage ICU on systems that have it,
  but will likely still require updating the width tables.  This is necessary to
  correctly compute advance width of e.g. combining emoji.
* Structure the table data to make future updates easier (this will mean giving
  up on inline comments, etc).
* Change the look-up tables to be multi-stage tables for performance as
  recommended by Unicode, particularly as we're increasing the table sizes.

## rlocale_data.h Update

In order to update Rlocale, we must first apply a patch
(`patches/patch-rloc-dat-init.txt`) to fix some minor bugs in the existing
tables, and then run `R/update-r-loc-dat.R`.

Unlike the Unicode tables, `rlocale_data.h` has specific widths for East Asian
(mostly) Ambiguous that are locale-dependent.  The update process does not touch
those at all, and only modifies the default locale values.  Entries to other
locales will be added where they did not exist previously.

We preserve the existing `ifdef` structures for win32, and preserve existing
comments as much as possible, at least for the widths table.  This in part means
that the resulting table is slightly less compact than it could be (we could
make it more compact but that further complicates already heinously complicated
logic).

The zero width table is regenerated completely, so inline comments there are
lost though those seem less important.

## Checks

We compare `nchar(..., type='width')` for every Unicode code starting at U+0001
with the following programs:

* glibc 2.29                (Unicode 11)
* utf8(1.2.0)::utf8_width   (Unicode 12.0, but github only)
* stri(1.4.6)::stri_width   (Unicode 10 - ICU4C 61.1)
* R 3.6.3                   (Unicode 8)
* R-devel 78347 w/ patch    (Unicode 12.1)

See [`R/check_uni.R`][16] for details.

## Width Assumptions

### Rules

In cascading order. Once a code point is matched its width is set and is no
longer eligible to be changed by subsequent rules:

* One Width:
    * U+00AD (Soft-Hyphen)
    * Prepending Marks
* Zero Width:
    * Code points with General Category Mn (non-spacing mark)
    * Code points with General Category Me (enclosing mark)
    * Code points with General Category Cf (format control)
    * Code points with General Category Cc (C0 or C1 controls, originally added
      by R)
* Two Width:
    * Code points that have East Asian Width Property "W" or "F"
    * Circled number "ideograms" from ARIB STD 24 (U+3248-324F)
    * I Ching hexagrams (U+4DC0-4DFF).
* Everything else in Unicode range is one width (note EAW specifies widths for
  some unassigned points)

The width assumptions intend to line up with what `glibc` does, except that
unspecified code points are computed per the above rules, whereas `glibc`
returns `-1` for them.

Other code calling the width routines should check for validity of code points
(e.g. no UTF-8 encoded UTF-16 surrogates).

See `process_width` in 'glibc(2.29)/localedata/unicode-gen/utf8_gen.py@220' in
the [glibc repo][14].

### Details On Non-Unicode Decisions

#### Glibc

Comments in the sources as well as some of the reported width values in the
pre-patch version strongly suggest `glibc` was used to generate or validate the
original tables, for this reason we make the same non-standard assumptions that
`glibc` makes, even though this may be controversial in some cases.

#### Soft Hyphen U+00AD

Soft hyphens are General Category Cf (Format Control) which should make them
width 0.  Whether this should be the case or not is controversial, but Jukka
Korpela provides a [compelling argument][11] for why they should be width 1,
particularly in the context of terminal emulators.

#### Prepending Marks

A subset of the General Category 'Cf' code points are
'Prepended_Concatenation_Mark' (PCM).  `glibc` and we treat these as width 1,
although this is questionable.  PCMs are also known as subtending marks because
they are used in e.g. Arabic scripts to extend under subsequent characters,
although in some cases they extend above.  In many ways these behave like
enclosing marks, except that because they enclose a possibly varying number of
**subsequent** characters (instead of a single **preceding** base character)
they end up in the 'Cf' category.

Some of the PCMs should be treated as non-spacing or enclosing marks.  For
example U+0605 is a supralinear mark over the subsequent numbers and [shouldn't
have advance width][2].  U+0600-0602 are less clear, with some [typographical
examples][3] suggesting that they should advance maybe one element
[or half or not][5] ([more examples][9]).  E.g. for the year sign Sanah, it
appears it has a spot reserved for the calendar designation (christian/arabic)
so it's zero advanced if it is specified, but if it isn't it advances
(debatable)[no advance][5]).  For the footnote marker it appears to
create an advance on the left, and the number mark an advance to the right,
maybe.  All of these as well as U+06DD were [originally][5] [proposed][8] as Gc
== 'Me'.

[U+0604][3] and [U+110bd][7] on subtend over following characters, but look like
more obviously like they should take up an advance element (respectively after
and before the subtended chars).

None of these to appear render correctly in browsers, OS X term, Iterm 2, with
most of them causing varying degrees of advance and overlap.  Every display I've
tried ignores the [Default Ignorable property][1] which should hide all these
code points if they are not supported...

PCMs also correlate close with the prepend [Grapheme Cluster Break Property][6].

#### Other Wide Characters

`glibc` treats a few characters as wide that others don't.  These include:

* Circled number ideograms e.g (10), (20), etc in U+3248-324F, which
  definitely look wide, but terminal does not display them wide.
* The I Ching hexagrams U+4DC0..4DFF. The glyphs seem to a little over 1 column
  wide on my terminal and browser, though the terminal places them as being 1
  wide.  Other sources support the wide display[10].

The number ideograms are from ARIB STD 24, and it makes sense that they are
considered wide by glibc, but what doesn't make sense is there are a lot more
of similar 2-byte ARIB STD 24 ideographs that look like they should be
rendered wide that are considered narrow by glibc (e.g. U+1F12F:1f169).
Should we really single out those 8 for special treatment?

Someone else also thinks this is a [bug][12].  And lots of controversy in a
[bug-report discussion][13].

[1]: http://unicode.org/L2/L2002/02368-default-ignorable.html
[2]: http://std.dkuug.dk/jtc1/sc2/wg2/docs/N3843.pdf
[3]: https://www.unicode.org/wg2/docs/n3734.pdf
[4]: https://www.unicode.org/L2/L2001/01428-arabic_enclosing_marks.pdf
[5]: https://www.unicode.org/review/pri310/pri310-background.html
[6]: https://www.unicode.org/L2/L2015/15183r-graph-cluster-brk.txt
[7]: https://www.unicode.org/L2/L2008/08400-kaithi-num-sign.pdf
[8]: https://www.unicode.org/wg2/docs/n2483.pdf
[9]: https://www.unicode.org/L2/L2001/01426-arabic_marks_examples.pdf
[10]: https://www.unicode.org/wg2/docs/n2363.pdf
[11]: http://jkorpela.fi/shy.html
[12]: https://sourceware.org/bugzilla/show_bug.cgi?id=24658
[13]: https://sourceware.org/bugzilla/show_bug.cgi?id=21750
[14]: https://www.gnu.org/software/libc/sources.html
[15]: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17755
[16]: https://github.com/brodieG/char-width/blob/master/R/check-uni.R

