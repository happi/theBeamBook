![PDF Build](https://github.com/happi/theBeamBook/actions/workflows/build.yml/badge.svg)
![Pages Build](https://github.com/happi/theBeamBook/actions/workflows/gh-pages.yml/badge.svg) 

# The BEAM Book

This is an attempt to document the internals of the Erlang runtime
system and the Erlang virtual machine known as the BEAM.

You can read or download the book as a PDF from the [latest
stable release](https://github.com/happi/theBeamBook/releases/latest)
or [online as a webpage](https://happi.github.io/theBeamBook/).

The book is written in AsciiDoc and most of it can be read directly
from source on GitHub in your browser. To read the book online just
open the file [book.asciidoc](book.asciidoc).

You can also read it as an [Github IO page](https://happi.github.io/theBeamBook/).

## Contributing

The plan is to make this book project into a collaboration effort so
that we can get a complete documentation of the Erlang runtime system
as soon as possible. Please feel free to contribute since this work is
far from done.

You can contribute by raising an issue, comment on open issues
or create a branch with a fix or an addition.

Note that the book is released under a Creative Commons license (see below)
and anything you contribute will also be included under that license.

The chapters in the book can be in one of four states:

1. Placeholder, basically only the title and perhaps an outline of the
chapter is done. If you are interested in writing the chapter or parts
of the chapter grab the corresponding issue and start writing.
2. First draft, most of the text is in place but more editing is needed.
Feel free to comment, focusing on missing content, hard to read passages,
the order of sections within the chapter, diagrams or pictures needed,
and plain errors.
3. Final draft, spelling and other errors probably still need fixing.
4. Done (for OTP version X), if things changes in later versions of
Erlang OTP the chapter will need an update.

(Not all chapters are yet marked in this way.)

### Style guide

There are several ways to use AsciiDoc and some constructs work
better in some environments or for some targets.

The priority of the AsciiDoc code in this project is that it
renders nicely for the following targets in the following order:
1. The PDF target
2. The HTML target
3. View directly on GitHub

We will try to come up with specific guides for which AsciiDoc
constructs to use and add them here as we discover what works
and what doesn't work.

<!-- #### The AsciiDoc dialect to use -->

#### Comments in AsciiDoc
Each chapter should begin with a comment about the status of
the chapter. This should be one of 'Placeholder', 'First Draft',
'Final Draft', or 'Done (for Erlang X.X)'.
There can also be a link to an issue describing what is needed
to bring the chapter to the next level.

A comment in the code starts with '//'.

<!-- #### Callouts
     What type of callout to use and for what (note, warning etc.)

-->

#### Linking to OTP/Erlang source code

When referring to the source code of Erlang/OTP please add
a link to a tagged version (not to master) of the code on GitHub,
as in:

----
<pre>
 link:https://github.com/erlang/otp/blob/OTP-19.1/erts/emulator/beam/erl_time.h[erl_time.h]
</pre>
----

#### Directory structure and build

Try to keep the root directory clean.

Put each chapter in a separate .asciidoc file in the chapters directory.
Use underscores "_" to separate words in chapter names but try to use
just one-word file names for the chapters.

Put code used in a chapter in code/CHAPTERNAME_chapter/src, and add an
include of the code in ap-code_listings.asciidoc.

Put images in the images directory.

#### How to tag chapters, sections, figures

The following is not yet done consistently so please feel
free to contribute by fixing tags in the current version.

Chapter tags should start with 'CH-'. Words in a tag are separated by
underscores '_'.

Part tags should start with 'P-'.

Section tags should start with 'SEC-'.

Figure tags should start with 'FIG-'.

Appendix tags should start with 'AP-'.

Code listing tags (in the appendix) should start with 'LISTING-'.

### Process

If you find something you do not understand or which is incorrect
please raise an issue in the [issue tracker](https://github.com/happi/theBeamBook/issues).

If you find spelling or formatting errors feel free to fix them and
just make a pull request.

For larger rewrites check the status of the chapter and check the
issues to see if someone is likely to be working on that chapter
right now. If someone else is working on the chapter try to contact
that person before doing a major rewrite. Otherwise either just go
ahead and do the rewrite and do a pull request or start by opening
an issue declaring what you intend to do.


## Building the PDF locally from source

The project contains a makefile which
will let you build your own PDF from the source, provided
that you have all the needed tools installed.

### Docker

You can build the project locally using docker by first building the docker image
```
make docker-build
```
And then building the project by
```
make docker
```

### Linux
WIP, to be updated
```shell
make
```

### Mac OSX

1. Install [asciidoc](https://github.com/asciidoctor/asciidoctor)
1. Install [asciidoctor-pdf](https://github.com/asciidoctor/asciidoctor-pdf)
1. Install [asciidoctor-diagram](http://asciidoctor.org/docs/asciidoctor-diagram/)
1. Install [ditaa](https://github.com/stathissideris/ditaa)
1. Install [graphviz](https://www.graphviz.org/)
1. Install [rouge](https://asciidoctor.org/docs/user-manual/#rouge)
1. Install [wget](https://www.gnu.org/software/wget/)
1. `make`

### Mac OSX (using brew etc)

1. `brew install asciidoctor`
1. `gem install asciidoctor-pdf`
1. `gem install asciidoctor-diagram`
1. `brew install ditaa`
1. `brew install graphviz`
1. `gem install rouge`
1. `brew install wget`
1. `make`

## License

_The Erlang Runtime System_ by Erik Stenman is licensed under a
Creative Commons Attribution 4.0 International License. Based on a
work at https://github.com/happi/theBeamBook.
A complete copy of the license can be found [here](LICENSE).


# A short and personal history of the book

I, Erik Stenman (Happi), started writing this book back in 2013.
At first I was thinking of self publishing the book on my blog,
but since English isn't my native language I felt I needed help
by a good editor.

I managed to get a deal with O'Reilly and started converting my
outline to their build process. My original plan was for a very long
and thorough book, which the editor felt would get few readers. I
started cutting my content and tried to write more of a tutorial than
a manual. Unfortunately progress was slow and pre-sales was even
slower and the publisher cancelled the book in 2015.

I managed to get a new deal with Pragmatic and started converting my
content to their build system and rewriting the book according to the
more pragmatic style of the new publisher, cutting down the content
even further. The series editor also wanted me to fit the book into
the Elixir series and I tried to add more Elixir examples. I did not
really manage to make it into an Elixir book and also my progress was
still slow, which led to another cancellation of the book early 2017.

Now I had three repositories with three different book building systems
with three different outlines of the book. In the end I more or less
went back to the original longer book outline and the original AsciiDoc
build system. I started a new repository in a private GitHub account and
started pulling in content from the three different versions.

Then on April 7 2017 I opened the repository to the public to share it with
some students. I didn't think anyone else would notice and I was not
planning to release the book for real yet since the repo currently
just contains bits and pieces from the different versions of the book.

There was more interest than I had expected though and fortunately
also several who where willing to contribute. From now on the book
is a collaborative effort to document the Erlang runtime system ERTS and the Erlang virtual machine BEAM,
and it is released with a Creative Commons license (see above).

Watch this space for further news and to see the whole book take shape.

-- Erik Stenman aka Happi

