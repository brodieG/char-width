The character display-column width tables R uses internally for `nchar(...,
type='width')` and other places were last updated in late 2015 to roughly
conform with Unicode 8.  Unicode 9 was released in June 2016, and it included a
substantial update to the East Asian Width annex, most notably the treatment of
emoji as wide (i.e. two column) characters.  Over the last couple of years many
(most?) terminal emulators have also updated their width tables, so it may make
sense to update R's computations as well.

Attached is a patch that updates the width tables in 'src/main/rlocale_data.h'
to Unicode 12.1.  I wrote an R script to do the updates programmatically, so it
should be straightforward to update to different versions[3].

To test I did this correctly I compared[4] the width computations for every Unicode
code starting at U+0001 with the following programs:

* glibc 2.29                (Unicode 11)
* utf8(1.2.0)::utf8_width   (Unicode 12.0, but github only)
* stri(1.4.6)::stri_width   (Unicode 10 - ICU4C 61.1)
* R 3.6.3                   (Unicode 8)
* R 4.0 r78116 w/ patch     (Unicode 12.1)

All tests were carried out after applying the separate patch proposed in
17755[1].  I did not update any of the locale-specific values (i.e. those not in
the first column), and attempted to preserve existing comments and file
structure as much as possible.

`glibc` takes a unique position on prepending marks (e.g. U+0600-U+0605), the
circled number "ideograms" from ARIB STD 24 (U+3248-324F), and the I Ching
hexagrams (U+4DC0-4DFF).  Comments in the existing R sources as well as existing
values for some points in those code ranges suggest `glibc` was a reference for
the original width tables, so I adopt the `glibc` position on those points.
This position is controversial[2].

These updates are only partial.  Other things that could/should be done:

* Update the other tables in 'rlocale_data.h'.
* Compute width on graphemes instead of Unicode code points; this is likely to
  be a bigger change that could possibly leverage ICU on systems that have it,
  but will likely still require updating the width tables.
* Structure the table data to make future updates easier (this will mean giving
  up on inline comments, etc).
* Change the look-up tables to be multi-stage tables for performance as
  recommended by Unicode, particularly as we're increasing the table sizes.

I've created a public repository[5] with the scripts to generate the updated
'rlocale_data.h' table as well as the scripts that check the width calculations
across several platforms.

[1]: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17755
[2]: https://sourceware.org/bugzilla/show_bug.cgi?id=21750
[3]: https://github.com/brodieG/char-width/tree/v0.1.0/R/update-rloc-dat.R
[4]: https://github.com/brodieG/char-width/tree/v0.1.0/R/check-uni.R
[5]: https://github.com/brodieG/char-width/tree/v0.1.0


