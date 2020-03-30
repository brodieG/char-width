The character display-column width tables R uses internally for `nchar(...,
type='width')` and other places were last updated in late 2015 to roughly
conform with Unicode 8.  Unicode 9 was released in June 2016, and it included a
substantial update to the East Asian Width annex, most notably the treatment of
emoji as wide (i.e. two column) characters.

That R uses outdated tables wasn't much of an issue because other software did
too.  For example, `glibc` didn't update to Unicode > 8 until August 2017
(v0.2.26 updated to Unicode 10, skipping 9), and my own OS X terminal probably
only updated in the past 18 months or so.  Now though it has been some time
since most software recognizes emoji as wide characters, so it may make sense to
update the tables.

Attached is a patch that updates the width tables in 'src/main/rlocale_data.h'
to Unicode 12.  I wrote an R script to do the updates programmatically, so it
should be straightforward to update to different versions.

The patches are an incomplete update in two ways:

First, I did not update any of the alpha, digit, etc. tables.  If there is
interest in doing so I can volunteer to look into it, though I'm reluctant to do
so without first seeing the level of interest in what I've already done.

Second, the display width of "characters" is not completely determined by the
East Asian Width tables because some "characters" are really graphemes made up
by combining several Unicode code points.  This is mostly an issue with emoji
sequences, as for most other graphemes the elements that are combined onto the
base character are zero width.  Resolving this will be more involved.  A
possible solution would be to use ICU where available (I think `BreakIterator`
might allow us to compute the graphemes).  However, that is beyond the scope of
this patch.  Nonetheless, this patch should resolve many common cases.

I only embarked on this project because I thought the patch would be
straightforward.  Of course I was wrong, primarily because the R tables contain
quite a bit more information than is present in Unicode tables, in particular
specific handling of the "Ambiguous" category across different locales and
Win32.  If an entry for a codepoint range already existed in the table, I only
updated the default locale.
