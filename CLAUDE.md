# CLAUDE.md — The BEAM Book

## Project

**The BEAM Book** — A deep dive into the Erlang runtime system internals.
by Erik Stenman (Happi)

Status: Published (1st edition). Uses AsciiDoc, not Markdown.

## Build

This project uses AsciiDoc with Asciidoctor. Requires Ruby and Bundler.

```bash
bundle install              # Install dependencies (one-time)
bundle exec make pdf-a4     # Build A4 PDF
bundle exec make epub       # Build EPUB
bundle exec make html       # Build HTML (GitHub Pages)
bundle exec make clean      # Remove generated files
```
