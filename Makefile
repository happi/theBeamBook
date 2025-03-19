ASSET_CHAPTERS = $(shell find chapters -type f)

.PHONY: all pdf pdf-a4 pdf-publish html docker docker-build clean serve

all: pdf-a4 html

pdf: pdf-a4

pdf-a4: beam-book-a4.pdf

pdf-publish: beam-book-publish.pdf

chapters/contributors.txt: .git
	git --no-pager log | git --no-pager shortlog -s -n | awk '{$$1=""}1' | grep -v "happi" | grep -v "Erik Stenman" | grep -v "Your Name" > $@

# A4 Format (Default)
beam-book-a4.pdf: chapters/opcodes_doc.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS)
	asciidoctor-pdf -r ./style/custom-pdf-converter.rb -r asciidoctor-diagram \
	-r ./style/custom-admonition-block.rb -a config=./style/ditaa.cfg \
	--doctype=book -a pdf-style=./style/pdf-theme.yml \
	-a pdf-width=595.28 -a pdf-height=841.89 \
	-a pdf-margin-top=0.75in -a pdf-margin-bottom=0.75in \
	-a pdf-margin-inner=0.75in -a pdf-margin-outer=0.5in \
	book.asciidoc -o $@

# Print-Ready 6"x9" for Publishing
beam-book-publish.pdf: chapters/opcodes_doc.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS)
	asciidoctor-pdf -r ./style/custom-pdf-converter.rb -r asciidoctor-diagram \
	-r ./style/custom-admonition-block.rb -a config=./style/ditaa.cfg \
	--doctype=book -a pdf-style=./style/pdf-theme-publish.yml \
	book.asciidoc -o $@ --trace


html: chapters/contributors.txt $(ASSET_CHAPTERS)
	cp -r images site
	asciidoctor -r asciidoctor-diagram -r ./style/custom-admonition-block.rb \
	-a config=style/ditaa.cfg --backend=html5 --doctype=book \
	-o site/index.html book.asciidoc --trace
	rsync -R code/*/*.png site

code/book/ebin/generate_op_doc.beam: code/book/src/generate_op_doc.erl
	erlc -o $(dir $@) $<

chapters/opcodes_doc.asciidoc: genop.tab code/book/ebin/generate_op_doc.beam
	erl -pa code/book/ebin/ -noshell -s generate_op_doc from_shell genop.tab chapters/opcodes_doc.asciidoc

genop.tab:
	wget -O genop.tab https://raw.githubusercontent.com/erlang/otp/master/lib/compiler/src/genop.tab
	touch $@

clean:
	find site -type f -name '.[^gitignore]*' -delete
	rm -f beam-book-*.pdf site/index.html site/*.png site/*.md5 xml/*.png xml/*.md5 xml/beam-book-from-ab.xml images/diag-*.png
	rm -rf site/code site/images .asciidoctor site/.asciidoctor

serve: all
	cd site && python3 -m http.server

docker:
	docker run -v .:/documents thebeambook/builder

docker-build:
	docker build --tag thebeambook/builder - < ./docker/Dockerfile
