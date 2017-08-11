(V)iew (L)arge table. Fast.
===========================

Installation
------------

First, you need to install [Stack](https://docs.haskellstack.org/en/stable/README/). After Stack is
installed, clone this repo, `cd` into it and run the following command in your
shell:

    $ make
    $ make install

This will build and install two binaries (`vl` and `vll`) into `~/.local/bin`.
Please make sure `~/.local/bin` is listed under `$PATH` and you are good to go. Run `vll
--help` in your shell for details.
