Possible Bug In Validation of UTF-8 Sequences

As per `?intToUtf8`, and in the comments to `valid_utf8`[1], it
appears R intends to prevent illegal UTF-8 such as UTF-8 encoded
UTF-16 surrogate pairs.  `R_nchar`, invoked via `base::nchar`,
explicitly validates UTF-8 strings[2], but allows the surrogate:

    > Encoding('\ud800')
    [1] "UTF-8"
    > nchar('\ud800')  // should be an error
    [1] 1

The problem manifests on systems where `char` is signed.  The logic
used to test for the forbidden sequences implicitly assumes that
`char` is unsigned[3]:

    if (c == 0xe0 && (d & 0x20) == 0) return 1;
    if (c == 0xed && d >= 0xa0) return 1;

Notice the `d >= 0xa0`.  On a system where `char` is signed this can
only ever be true if a byte has more than 8 bits, as otherwise the
maximum value of `d` is 0x7f because `d` is retrieved from a plain
`char` pointer[4]:

    if (((d = *(++p)) & 0xc0) != 0x80) return 1;

Where `p` is defined as[5]:

     const char *p;

In contrast `c` above is correctly cast to `unsigned char` prior to
use[8]:

     c = (unsigned char)*p;

I attach a simple patch to address this.

I also include a patch to remove the handling of surrogates from
`R_nchar` as that should not longer be necessary, and additionally the
current handling appears incorrect.  AFAICT, the current handling
attempts to decode a surrogate pair by combining the high surrogate
with the same high surrogate, instead of the high surrogate with the
subsequent character that hopefully is the low surrogate[7].

Here is some code that could be added to regression tests:

    surr_good <- '\ud840\udc00'            # auto-converts to normal
    surr_bad <- paste0('\ud840', '\udc00') # surrogates remain
    good <- c('hello', 'world', surr_good, '\ud7ff', '\ue000', '\U0010ffff')
    bad <- c(surr_bad, '\ud800', '\udfff', '\U00110000')

On R3.6.3:

    nchar(good, allowNA=TRUE)
    [1] 5 5 1 1 1 1
    nchar(bad, allowNA=TRUE)
    [1] 2 1 1 1

On R-devel (2020-03-31 r78116) w/ patch:

    nchar(good, allowNA=TRUE)
    [1] 5 5 1 1 1 1
    nchar(bad, allowNA=TRUE)
    [1] NA NA NA NA

I ran `make check-devel` successfully, although I did have to suppress
one PCRE test that segfaults on my particular set-up, though that
segfaulted prior to my patch as well.

The patch does not prevent the creation of illegal UTF-8 strings,
although I imagine it would be straightforward to add checks to the
entry points into CHARSXPs if that were desired.

Finally, this signed char business hints at a potential broader issue.
If I understand correctly importing byte sequences with values greater
than 0x7f overflows the `char` buffers on systems with signed chars
and octet (or lesser) bytes, e.g. as in `do_readLines`[6] where an
integer procured via `fgetc` is subsequently stored in a `char`
buffer.  Obviously this hasn't mattered much to date, presumably
because the implementations R runs on define the `unsigned char` to
`signed char` conversion in such a way that the `signed char` to
`unsigned char` conversion restores the original value.  I don't know
if this is something explicitly checked for like the `int` == 32bits
assumption.

[1]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/valid_utf8.h#L61
[2]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/character.c#L148
[3]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/valid_utf8.h#L106
[4]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/valid_utf8.h#L84
[5]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/valid_utf8.h#L69
[6]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/connections.c#L3935
[7]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/character.c#L184
[8]: https://github.com/wch/r-source/blob/tags/R-3-6-3/src/main/valid_utf8.h#L73


