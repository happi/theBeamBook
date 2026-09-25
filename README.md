# The BEAM Book

The first-edition manuscript is in [book.asciidoc](book.asciidoc), with
examples in [code/](code/). [Read it online](https://happi.github.io/theBeamBook/)
or [download the PDF](https://github.com/happi/theBeamBook/releases/latest/download/beam-book-a4.pdf).

[Second-edition examples and companion notes](second-edition/) are also available.

## Build the first edition

Install Ruby, Bundler, Erlang, Java, Graphviz and rsync, then run:

```sh
bundle install
make pdf-a4 html
```

Outputs are `beam-book-a4.pdf` and `site/index.html`. Use `make pdf-publish`
for the print PDF and `make epub` for EPUB.

## Licence

The first edition and code examples use [CC BY 4.0](LICENSE).
The second-edition companion carries its own copyright and licence notices.
Existing third-party notices apply to their respective files.
