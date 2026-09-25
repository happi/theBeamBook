# BB-201: First-edition source references

The first edition now marks prose source references with named AsciiDoc source footnotes. References retain their original targets, apart from the leex correction described below, including historical OTP tags, branches, line anchors and HTTP URLs. The renderer uses the matching record in `data/first-edition-sources.json` to print a title, project or author, relevant file or section, and a readable URL.

## Counts and preservation

- 76 source-citation occurrences use 73 distinct source IDs across 18 chapter and appendix files.
- All 73 source targets have bibliographic metadata. Three repeated citations reuse an existing chapter-local source ID.
- Source meanings, historical qualifications, code blocks and explicit anchors are preserved. The link labelled “leex manual” now points to leex, correcting its previous yecc target.
- All fenced and listing code blocks are unchanged, including clone commands, download commands and captured debugger output.
- All explicit anchors remain unchanged. Existing bibliography citations and explanatory footnotes retain their meaning.

| Source file | Citation occurrences |
|---|---:|
| `chapters/introduction.asciidoc` | 2 |
| `chapters/compiler.asciidoc` | 9 |
| `chapters/processes.asciidoc` | 6 |
| `chapters/type_system.asciidoc` | 2 |
| `chapters/beam.asciidoc` | 4 |
| `chapters/beam_modules.asciidoc` | 2 |
| `chapters/calls.asciidoc` | 1 |
| `chapters/beam_loader.asciidoc` | 10 |
| `chapters/scheduling.asciidoc` | 2 |
| `chapters/memory.asciidoc` | 5 |
| `chapters/gc.asciidoc` | 1 |
| `chapters/io.asciidoc` | 4 |
| `chapters/distribution.asciidoc` | 4 |
| `chapters/c.asciidoc` | 8 |
| `chapters/ops.asciidoc` | 4 |
| `chapters/debugging.asciidoc` | 6 |
| `chapters/testing.asciidoc` | 3 |
| `chapters/building.asciidoc` | 3 |

## Reference data

The metadata keys are the full, unchanged source URLs. GitHub records print a concise repository URL and identify the file, revision and line range separately. The digital hyperlink retains the full deep link. Existing `maint`, `master`, OTP-19.1, OTP-19.3, OTP-20.0 and OTP-23.0 qualifications remain visible. No DOI references occur in the converted first-edition source set.

Titles and attribution use the existing manuscript, source-file paths and primary publication evidence. Additional primary checks included [Björn Gustavsson’s compiler history](https://www.erlang.org/blog/beam-compiler-history/), [EEP 18](https://www.erlang.org/eeps/eep-0018.html), [Fred Hébert’s Heroku article](https://www.heroku.com/blog/logplex-down-the-rabbit-hole/), [Maxim Fedorov’s GDB article](https://max-au.com/2022/03/29/debugging-the-beam/), and [the publisher’s PropEr book page](https://pragprog.com/titles/fhproper/property-based-testing-with-proper-erlang-and-elixir/). The title and report number for M. Pettersson’s *A staged tag scheme for Erlang* are confirmed in the references of [Aronis and Sagonas’s paper](https://user.it.uu.se/~kostis/Papers/erlang12_sharing.pdf). Original source targets remain unchanged even where an old website now redirects or is unavailable.

## Special cases

The EEP 18 reference previously appeared inside an explanatory footnote. Its explanatory wording remains, with the source citation immediately after the explanation marker to avoid nested footnotes. Two `erts_alloc` quotations retain their text and attribution; their introductory sentences now cite the same chapter-local source entry. The indented `compile.erl` source link was moved out of a literal paragraph so its citation can render as a reference.

The Erlang download page remains a convenience link, with its URL printed in the prose. Commands and captured URLs remain literal examples. Copyright, contributor and book-download links in front matter are already printed as URLs and are outside the chapter edits. Inactive chapter files are unchanged.

The Compiler chapter described its `yecc.html` target as the “leex manual.” The citation now points to https://www.erlang.org/doc/apps/parsetools/leex.html and identifies the lexical analyzer generator.

Source checks passed: every citation ID resolves to its intended URL, reused IDs follow their definition, every URL has metadata, and code blocks and explicit anchors are preserved. PDF and EPUB validation is performed by the integrated build.
