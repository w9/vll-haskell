(V)iew (L)arge table. Fast.
===========================

<a href="https://asciinema.org/a/03hTuK0wksayMLFjWFEmgKrZF" target="_blank">
  <img src="https://asciinema.org/a/03hTuK0wksayMLFjWFEmgKrZF.png" />
</a>


Binary installation
-------------------

Grab the binaries from
the [releases](https://github.com/w9/vll-haskell/releases) page and put it in
your PATH -- profit!

There are two binaries in the releases: `vl` and `vll`

  * `vl` is the main binary, it takes one or more filenames (or stdin) as the
    input and outputs the formatted table into stdout
  * `vll` is a simple wrapper on `vl`, it's equivalent to `vl -z ... | less -SR`


Get help
--------

    $ vl --help


Features
--------

**VLL** tries to load a fixed number of lines to calculate the appropriate
column width, and then immediately output the formatted table. This makes it
particularly suitable for viewing large ascii tables.

**VLL** guesses the format by the file's extension. If the file has extension
".csv", it is treated as a CSV file, otherwise a TSV file. The latter
includes the cases where the input is *piped* in.

A CSV/TSV file is assumed to *strictly* follow [RFC 4180](https://tools.ietf.org/html/rfc4180).
If you encounter a parse error, usually it's because the file doesn't follow
the standard strictly, (e.g., its use of quotes or escapse sequences doesn't
comply with the standard,) in which case the `--naive` option fixes most
problems. If the file complies RFC 4180, **VLL** escapses special characters
like `\t`, `\r` or `\n` in cells.

One of the features I like about **VLL** is that it displays comments (lines
preceded by "#" by default) along with the formatted table. This is especially
useful when the CSV file has an informative header, or sometimes when the
comments separates the CSV file into sections.

Don't forget that you can always use `vl` in a pipe chain.


Build from source
-----------------

First, you need to install [Stack](https://docs.haskellstack.org/en/stable/README/).

If you have Stack already, clone this repo, `cd` into it and run the following
command in your shell:

    $ make
    $ make install

This will build and install two binaries (`vl` and `vll`) into `~/.local/bin`.
Make sure `~/.local/bin` is listed under `$PATH` and you are good to go. Run `vll
--help` in your shell for details.
