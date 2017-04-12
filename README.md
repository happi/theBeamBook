[![Build Status](https://travis-ci.org/happi/theBeamBook.svg?branch=master)](https://travis-ci.org/happi/theBeamBook)

# Hi

I opened this repo to the public today (April 7) to share it with
some students. I was giving a lecture about the BEAM at Chalmers. I
didn't think anyone else would notice. I was not planning to release
this for real yet since the repo currently just contains bits and
pieces from several different versions of the book that I have been
writing.

I hope to bring in more chapters and fill out the existing chapters in
the weeks to come, but please feel free to comment and correct any
errors you find. The plan is to make this into a collaborate effort so
that we can get a complete documentation of the Erlang Runtime system
as soon as possible.

Anyway, it is finally out in the public, warts and all.

Welcome to the site, please contribute.

# About this book
A description of the Erlang Runtime System ERTS and the virtual
Machine BEAM.

This is an attempt to document how the internals of the Erlang Runtime
System work.

Please feel free to contribute since this work is far from done, and
it will probably never be done since there is continuous development
of the Erlang runtime system.

The book is written in AsciiDoc and most of it can be read directly
from source on github in your browser. To read the book online just
open the file [book.asciidoc](book.asciidoc).

You can also read or download the book as a PDF from the [latest
stable release](https://github.com/happi/theBeamBook/releases/latest).

Alternatively you can create your own PDF from the source code
following the instructions below.

## Building from source

### Docker

A Docker image containing everything
which is needed to build the book from source is provided. To use it:

```shell
docker pull robertoaloi/docker-thebeambook:latest
docker run -v $PWD:/book -t robertoaloi/docker-thebeambook make
```

### Linux

```shell
make
```

### Mac OSX

1. Install [MacTex](http://www.tug.org/mactex/). Note that you would
   need to _full_ MacTex rather than just BasicTex.
1. Add `/Library/TeX/texbin` to your `PATH`.
1. `easy_install dblatex`
1. Install [asciidoc](http://asciidoc.org/INSTALL.html)
1. Install [shaape](https://github.com/christiangoltz/shaape)
1. Install [source-highlight](https://www.gnu.org/software/src-highlite/)
1. Install [wget](https://www.gnu.org/software/wget/)
1. `make`

### Mac OSX (using brew)

1. `brew install Caskroom/cask/mactex`
1. Add `/Library/TeX/texbin` to your `PATH`.
1. `sudo easy_install dblatex`
1. `brew install asciidoc`
1. `brew install pygtk py2cairo pango`
1. `pip install shaape`
1. `brew install source-highlight`
1. `brew install wget`
1. `make`

## License

_The Erlang Runtime System_ by Erik Stenman is licensed under a
Creative Commons Attribution 4.0 International License. Based on a
work at https://github.com/happi/theBeamBook.
A complete copy of the license can be found [here](LICENSE).
