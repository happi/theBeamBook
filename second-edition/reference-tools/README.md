# Book source references

This directory contains the shared reference formatter, bibliographic facts and
checks for both editions. It contains no second-edition manuscript.

Write a source as `footnote:source-chapter-name[https://example.org/article]` and
reuse it as `footnote:source-chapter-name[]`. Give it a descriptive, unique ID;
keep that ID when moving the surrounding prose. Set the document attribute
`:source-catalog:` to the relevant JSON filename in `data/`.

Each catalogue record supplies `title`, `project`, `section` and `display_url`.
The key is the full digital URL, including its revision and fragment. GitHub
entries print the repository URL and identify the file, revision and lines in
the section field. DOI entries print `https://doi.org/...`. Builds use these
checked-in facts and fail on missing metadata. They never fetch metadata.

The PDF adapter creates named destinations from the source IDs, chapter-local
`[S1]` labels, chapter-end source lists and return links. It keeps short entries
together across page breaks. Explanatory notes have a separate counter.
The HTML/EPUB adapter relocates sources using a DOM parser and repairs links
between EPUB chapter files. Headings, bibliography entries and navigation
remain under the book converter's control. The second edition's existing
page-footnote and digital-note adapters use the same catalogue formatter.

Relative PDF companion links become PDF `GoToR` actions with separate file and
named-destination fields. `package_release.py` refuses to package a release
with a missing companion. Distribute the ZIP and preserve filenames when
extracting it. The second-edition EPUB contains the optional deep dives.

## Checks

Install the book's Ruby bundle and Python `PyMuPDF`. From the repository root:

```sh
bash second-edition/reference-tools/test/run.sh
python3 second-edition/reference-tools/check_references.py beam-book-a4.pdf beam-book-publish.pdf beam-book.epub
bundle exec ruby second-edition/reference-tools/check_html.rb site/index.html
```

`BUNDLE` can name a Bundler executable. In the private second-edition repository,
run `make reference-release` and check `beam-book-2nded.pdf`,
`implementation-deep-dives.pdf` and `beam-book.epub`. Validate both site HTML
files together with `check_html.rb`. Run EPUBCheck separately for EPUB schema
conformance. The checkers inspect destinations, files and labels; they do not
claim that every remote website remains available.

The regression fixture covers repeated references, chapter numbering, GitHub
deep links, readable DOI text and remote PDF destinations. The fixture uses
synthetic source metadata and is not part of either book.
