(V)iew (L)arge table. Fast.
===========================

Binary installation
-------------------

Grab the binaries from
the [releases](https://github.com/w9/vll-haskell/releases) page and put it in
your PATH -- profit!

Get help
--------

    $ vl --help

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
