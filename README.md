guile-commonmark
================
guile-commonmark is a library for parsing [CommonMark](http://commonmark.org/),
a fully specified variant of Markdown.


Example
-------
```scheme
(use-modules (commonmark sxml)
             (sxml simple))

(define doc
  "A CommonMark document
=============

1. here is a list
2. with another item

    this is some code

A regular paragraph")

;; Parses the CommonMark.
(define doc-sxml (commonmark->sxml doc))

;; Writes to the current output port
(sxml->xml doc-sxml)
```

Requirements
------------

 - [GNU Guile](https://www.gnu.org/software/guile/) >= 2.0.11
 - [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config)

Installation
------------

Download the latest tarball and run:
```sh
./configure
make
sudo make install
```

This will install guile-commonmark with the prefix `/usr/local/`. This
is not in the default load path for GNU Guile. You may choose to
change the prefix to your GNU Guile's location with `./configure
--prefix=/usr` or add `/usr/local/` to GNU Guile's load path in your
`.profile` or `.bash_profile` like this (replacing 2.2 with your GNU
Guile version):
```sh
export GUILE_LOAD_PATH="/usr/local/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_COMPILED_LOAD_PATH"
```

You can manually specify the locatation of GUILE using `./configure GUILE=$(which guile)` if you run into the following error -
```sh
configure: error: 'guild' binary not found; please check your guile-2.x installation.
```


To build from git requires Autoconf and Automake.
```sh
./bootstrap
./configure
make
make install
```

To install from a recent version of git using Guix.
```sh
guix package -f guix.scm
```

License
-------
LGPLv3 or later. See COPYING.LESSER and COPYING
