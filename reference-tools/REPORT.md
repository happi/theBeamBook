# BB-201: Reference-system repair

The source formatter now gives every named source a stable destination derived
from its AsciiDoc ID. Chapter-local `[S1]` labels link to the matching chapter-end
entry. Entries print the project or author, title, relevant section/file and URL.
GitHub entries print a concise repository URL plus revision/path information;
the hyperlink retains its full deep target. DOI entries print `https://doi.org/`.

The shared implementation and reference data live in `reference-tools/`
in the public first-edition repository. The private manuscript uses a copy of
these helpers. No second-edition manuscript is included in the public change.
Upstream's newer public code examples and README were preserved unchanged.

## Verified counts

| Output | Pages | Source entries | Inline source citations |
|---|---:|---:|---:|
| First-edition A4 PDF | 309 | 73 | 76 |
| First-edition print PDF | 477 | 73 | 76 |
| First-edition EPUB | n/a | 73 | 76 |
| Second-edition interior PDF | 410 | 116 | 117 |
| Second-edition assembled PDF | 413 | 116 | 117 |
| Optional companion PDF | 185 | 80 | 80 |
| Second-edition EPUB, including companion | n/a | 165 | 171 |

These are rendered occurrences, not distinct web URLs. PDF and EPUB wrappers
include different appendix content. Shared material also appears in the separate
companion, so counts across formats should not be summed as distinct sources.
The first edition adds source markers to existing prose references; the second
edition upgrades existing markers to stable IDs and descriptive source entries.

## Companion delivery

The private `make reference-release` target creates
`output/beam-book-2nded-release.zip`, containing `beam-book-2nded.pdf`,
`implementation-deep-dives.pdf`, `beam-book.epub` and extraction instructions.
`make release-2nded` builds this bundle too. Distribute the ZIP to PDF readers;
extract both PDFs into the same directory without renaming them. Sending the
main PDF alone would still omit its companion.

The converter emits proper PDF GoToR actions, with separate filename and named
destination fields. All 83 main-to-companion and 63 companion-to-main annotations
resolve. Links visibly name *Implementation Deep Dives*, the relevant subject,
and its optional implementation-detail status. EPUB deep dives stay inside the
book's navigation. The package step rejects missing companion files.

## Verification

- Both editions: full PDF, EPUB and HTML builds completed. No generated PDF was
  manually patched.
- Every PDF source marker's label matches its destination entry; every named
  destination and local companion destination resolves. PDF outlines remain.
- Both EPUBs pass EPUBCheck with zero errors and zero warnings. All package-local
  links, fragments, source labels and navigation targets resolve.
- Both sites pass checks for missing files, fragments and mismatched source labels.
- The source regression fixture passes chapter-counter, repeated-reference,
  full GitHub URL, readable DOI and remote-PDF destination checks.
- The private edition's eight unit tests and complete PDF-note layout regression
  suite pass, including page-edge notes, code frames and 144 punctuation cases.
- Rendered TOCs, chapter openings, code examples and source pages in both editions
  were inspected. Entries remain legible, with printed URLs and no clipping.
  Source entries now stay together where they fit on a page.

First-edition prose meanings, code blocks, historical qualifications and explicit
anchors are preserved. One existing incorrect target was corrected: “leex manual”
previously linked to yecc. Four image descriptions containing commas were quoted
so EPUB generation preserves the full alt text instead of treating words as image
widths. The EPUB body uses an include tag instead of a fragile line number.
Two existing first-edition alphabetic-list warnings and the PDF theme deprecation
warning remain; they do not affect link or EPUB validation. Remote site uptime is
not guaranteed by offline link checks.

## Rebuilt outputs

Relative to the first-edition task checkout:

- `beam-book-a4.pdf`
- `beam-book-publish.pdf`
- `beam-book.epub`
- `site/index.html`

Relative to the private second-edition task checkout:

- `beam-book-2nded-content.pdf`
- `beam-book-2nded.pdf`
- `implementation-deep-dives.pdf`
- `beam-book.epub`
- `output/beam-book-2nded-release.zip`
- `site/index.html` and `site/implementation-deep-dives.html`

## Changed files

### Public first edition

- `Makefile`
- `book.asciidoc`
- `chapters/beam.asciidoc`
- `chapters/beam_loader.asciidoc`
- `chapters/beam_modules.asciidoc`
- `chapters/building.asciidoc`
- `chapters/c.asciidoc`
- `chapters/calls.asciidoc`
- `chapters/compiler.asciidoc`
- `chapters/debugging.asciidoc`
- `chapters/distribution.asciidoc`
- `chapters/gc.asciidoc`
- `chapters/introduction.asciidoc`
- `chapters/io.asciidoc`
- `chapters/memory.asciidoc`
- `chapters/ops.asciidoc`
- `chapters/processes.asciidoc`
- `chapters/scheduling.asciidoc`
- `chapters/testing.asciidoc`
- `chapters/type_system.asciidoc`
- `epub-book.asciidoc`
- `reference-tools/BB-201-first-data.md`
- `reference-tools/BB-201-second-data.md`
- `reference-tools/README.md`
- `reference-tools/check_html.rb`
- `reference-tools/check_references.py`
- `reference-tools/data/first-edition-sources.json`
- `reference-tools/data/second-edition-sources.json`
- `reference-tools/digital_sources.rb`
- `reference-tools/package_release.py`
- `reference-tools/pdf_sources.rb`
- `reference-tools/source_catalog.rb`
- `reference-tools/test/reference-fixture.json`
- `reference-tools/test/references.adoc`
- `reference-tools/test/run.sh`
- `reference-tools/test/verify_fixture.py`
- `style/custom-pdf-converter.rb`

### Private second edition

- `Makefile`
- `book.asciidoc`
- `chapters/01_introduction.asciidoc`
- `chapters/02_compiler.asciidoc`
- `chapters/03_processes.asciidoc`
- `chapters/04_type_system.asciidoc`
- `chapters/05_beam.asciidoc`
- `chapters/06_beam_modules.asciidoc`
- `chapters/09_beam_loader.asciidoc`
- `chapters/10_jit.asciidoc`
- `chapters/11_scheduling.asciidoc`
- `chapters/12_memory.asciidoc`
- `chapters/14_data_structures.asciidoc`
- `chapters/15_io.asciidoc`
- `chapters/16_distribution.asciidoc`
- `chapters/17_c.asciidoc`
- `chapters/18_ops.asciidoc`
- `chapters/19_debugging.asciidoc`
- `chapters/22_profiling.asciidoc`
- `chapters/23_tweak.asciidoc`
- `epub-book.asciidoc`
- `implementation-deep-dives.asciidoc`
- `reference-tools/BB-201-first-data.md`
- `reference-tools/BB-201-second-data.md`
- `reference-tools/README.md`
- `reference-tools/check_html.rb`
- `reference-tools/check_references.py`
- `reference-tools/data/first-edition-sources.json`
- `reference-tools/data/second-edition-sources.json`
- `reference-tools/digital_sources.rb`
- `reference-tools/package_release.py`
- `reference-tools/pdf_sources.rb`
- `reference-tools/source_catalog.rb`
- `reference-tools/test/reference-fixture.json`
- `reference-tools/test/references.adoc`
- `reference-tools/test/run.sh`
- `reference-tools/test/verify_fixture.py`
- `style/custom-pdf-converter.rb`
- `style/digital-notes.rb`
- `test/validate_pdf_notes.py`

