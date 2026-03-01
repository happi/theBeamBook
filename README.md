![Pages Build](https://github.com/happi/theBeamBook/actions/workflows/gh-pages.yml/badge.svg)
![PDF Build](https://github.com/happi/theBeamBook/actions/workflows/build-pdf.yml/badge.svg)

[Download latest PDF](https://github.com/happi/theBeamBook/releases/latest/download/beam-book-a4.pdf)

# The BEAM Book

This repository contains the latest free and open edition of The BEAM Book,
a deep dive into the internals of the Erlang runtime system and its virtual
machine, the BEAM.

The current open edition is **1st Edition** (2025), licensed under CC BY 4.0.

You can read the book as a [PDF](https://github.com/happi/theBeamBook/releases/latest),
[browse it online](https://happi.github.io/theBeamBook/), or read the
AsciiDoc source directly on GitHub starting from [book.asciidoc](book.asciidoc).

The printed edition is available on Amazon:
[US](https://www.amazon.com/dp/9153142535) |
[UK](https://www.amazon.co.uk/dp/9153142535) |
[DE](https://www.amazon.de/dp/9153142535) |
[SE](https://www.amazon.se/dp/9153142535)

## Edition Policy

This book follows a rolling open model.

- The newest edition is published commercially.
- The previous edition is released publicly in this repository.
- Over time, all major editions become open.

This means the open version may lag behind the latest printed edition.
The open version is stable and complete, but may not contain the newest
chapters or revisions.

## Contributing

Contributions are welcome. Raise an issue, comment on open threads, or
open a pull request.

By submitting a pull request, you agree that your contribution is licensed
under the same license as this repository (CC BY 4.0). Contributions may
appear in future commercial editions of the book with attribution.

Each chapter can be in one of four states:

1. **Placeholder** -- only a title and maybe an outline.
2. **First draft** -- the text is mostly there but needs editing.
3. **Final draft** -- typo-hunting and polishing.
4. **Done (for OTP X)** -- until a newer OTP version changes things.

For larger rewrites, check the chapter's status and existing issues.
Coordinate with any active authors before heavy edits.

### Style guide

AsciiDoc constructs should render nicely in this order of priority:

1. PDF output
2. HTML output
3. Direct view on GitHub

#### Linking to OTP source code

Always link to a tagged OTP version, e.g.

```
link:https://github.com/erlang/otp/blob/OTP-19.1/erts/emulator/beam/erl_time.h[erl_time.h]
```

#### Directory structure

- Chapters live under `chapters/` in their own `.asciidoc` files.
- Code samples go in `code/CHAPTERNAME_chapter/src`.
- Images sit in `images/`.

#### Tag conventions

- Chapters: `CH-...`
- Parts: `P-...`
- Sections: `SEC-...`
- Figures: `FIG-...`
- Appendices: `AP-...`
- Code listings: `LISTING-...`

## Commercial Editions

Printed and Kindle editions help fund continued work on this book.
If you want the most recent version and professionally typeset formats,
consider purchasing the latest edition.

## Building the PDF locally

A `Makefile` builds both PDF and HTML. With dependencies installed:

```shell
bundle install
bundle exec make pdf-a4
```

Docker users can avoid local installs:

```shell
make docker-build
make docker
```

### Manual dependencies

#### Linux (Debian/Ubuntu)

```shell
sudo apt install git rsync wget curl make \
                 ruby ruby-dev default-jre \
                 asciidoctor graphviz
bundle install
bundle exec make pdf-a4
```

#### macOS + Homebrew

```shell
brew install asciidoctor graphviz wget ditaa ruby
export PATH=$(brew --prefix ruby)/bin:$PATH
bundle install
bundle exec make pdf-a4
```

## Licence

*The Erlang Runtime System* by Erik Stenman is licensed under
**Creative Commons Attribution 4.0 International (CC BY 4.0)**.
See [LICENSE](LICENSE) for details.
