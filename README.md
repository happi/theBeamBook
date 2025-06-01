![Pages Build](https://github.com/happi/theBeamBook/actions/workflows/gh-pages.yml/badge.svg) 
![PDF Build](https://github.com/happi/theBeamBook/actions/workflows/build-pdf.yml/badge.svg)

[Download latest PDF](https://github.com/happi/theBeamBook/releases/latest/download/beam-book-a4.pdf)

# The BEAM Book

**1.0 — First Edition now *in print***

Grab the tree‑ware from your local Amazon store:

* 🇺🇸 [US](https://www.amazon.com/dp/9153142535)
* 🇬🇧 [UK](https://www.amazon.co.uk/dp/9153142535)
* 🇩🇪 [DE](https://www.amazon.de/dp/9153142535)
* 🇸🇪 [SE](https://www.amazon.se/dp/9153142535)

This project documents the internals of the Erlang runtime system and its virtual machine, the **BEAM**.

You can read or download the book as a PDF (current stable release **1.0 – First Edition**) from the [latest stable release](https://github.com/happi/theBeamBook/releases/latest) or [browse it online](https://happi.github.io/theBeamBook/).

The book is written in AsciiDoc and most chapters can be read directly from source on GitHub. Open [book.asciidoc](book.asciidoc) for a quick start.

You can also read it as a [GitHub Pages site](https://happi.github.io/theBeamBook/).

## Contributing

The goal is a collaborative, authoritative reference on the Erlang runtime system. Please contribute—this work is far from complete.

* Raise an issue, comment on open threads, or open a pull request.
* All contributions fall under the project’s Creative Commons licence (see below).

Each chapter can be in one of four states:

1. **Placeholder** — only a title and maybe an outline. Grab the matching issue and start writing.
2. **First draft** — the text is mostly there but needs editing: missing content, clarity, diagram tweaks, etc.
3. **Final draft** — typo‑hunting and polishing.
4. **Done (for OTP X)** — until a newer OTP version changes things.

### Style guide

AsciiDoc constructs should render nicely in this order of priority:

1. PDF output
2. HTML output
3. Direct view on GitHub

We’ll accumulate specific do’s and don’ts here as we learn what works.

#### Comments in AsciiDoc

Each chapter starts with a comment marking its status (`// Placeholder`, `// First Draft`, etc.) and possibly a link to an issue that tracks next steps.

#### Linking to OTP/Erlang source code

Always link to a *tagged* OTP version, e.g.

```
link:https://github.com/erlang/otp/blob/OTP-19.1/erts/emulator/beam/erl_time.h[erl_time.h]
```

#### Directory structure and build

* Chapters live under `chapters/` in their own `.asciidoc` files (use `_` for word breaks).
* Code samples go in `code/CHAPTERNAME_chapter/src` and are included via `ap-code_listings.asciidoc`.
* Images sit in `images/`.

#### Tag conventions

* Chapters: `CH-…`
* Parts: `P-…`
* Sections: `SEC-…`
* Figures: `FIG-…`
* Appendices: `AP-…`
* Code listings: `LISTING-…`

### Process

Found something odd or incorrect? Open an issue in the [tracker](https://github.com/happi/theBeamBook/issues).

Spotted a typo? File a quick PR.

For larger rewrites, check the chapter’s status and existing issues. Coordinate with any active authors before heavy edits.

---

## Building the PDF locally from source

A `Makefile` builds both PDF and HTML. With dependencies installed, run:

```shell
make
```

Result: `beam-book.pdf` in the project root and an HTML copy under `site/`.

Docker users can avoid local installs:

```shell
make docker-build  # build the image
make docker        # build the book inside the container
```

Devcontainer aficionados: open the repo in your IDE’s devcontainer and just run `make`.

### Manual dependencies

See platform‑specific instructions below if you prefer native builds.

#### Linux (Debian/Ubuntu)

```shell
sudo apt install git rsync wget curl make \
                 ruby ruby-dev default-jre \
                 asciidoctor graphviz
sudo gem install asciidoctor-pdf asciidoctor-diagram rouge
make
```

#### macOS + Homebrew

```shell
brew install asciidoctor graphviz wget ditaa
sudo gem install asciidoctor-pdf asciidoctor-diagram rouge
make
```

## Licence

*The Erlang Runtime System* by **Erik Stenman** is licensed under **CC BY 4.0**. See [LICENSE](LICENSE) for details.

---

# A short and personal history of the book

I, Erik Stenman (Happi), started writing this book back in 2013.
After twelve years of starts, stops, and rewrites, version 1.0
ships—less because the BEAM has stopped evolving, and more because
it felt time to draw a line in the sand.

At first, I was thinking of self‑publishing the book on my blog,
but since English isn't my native language, I felt I needed help
from a good editor.

I managed to get a deal with O'Reilly and started converting my
outline to their build process. My original plan was for a very long
and thorough book, which the editor felt would get few readers. I
started cutting my content and tried to write more of a tutorial than
a manual. Unfortunately, progress was slow, and pre-sales were even
slower, and the publisher cancelled the book in 2015.

I managed to get a new deal with Pragmatic. I started converting my
content to their build system and rewriting the book according to the
more pragmatic style of the new publisher, cutting down the content
even further. The series editor also wanted me to fit the book into
the Elixir series, and I tried to add more Elixir examples. I did not
manage to make it into an Elixir book, and my progress was
still slow, which led to another cancellation of the book in early 2017.

Now I had three repositories with three different book-building systems
with three different outlines of the book. In the end, I more or less
went back to the original longer book outline and the original AsciiDoc
build system. I started a new repository in a private GitHub account and
started pulling in content from the three different versions.

Then on April 7, 2017, I opened the repository to the public to share it with
some students. I didn't think anyone else would notice, and I was not
planning to release the book for real yet since the repo currently
contains bits and pieces from the different versions of the book.

There was more interest than I had expected, though, and fortunately,
also several who were willing to contribute. From now on, the book
is a collaborative effort to document the Erlang runtime system, ERTS
and the Erlang virtual machine BEAM,
and it is released with a Creative Commons license (see above).

Watch this space for further news and to see the whole book take shape.

-- Erik Stenman aka Happi

