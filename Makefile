ASSET_CHAPTERS = $(shell find chapters -type f)
VERSION = 1.0.$(shell git rev-list v1.0..HEAD --count)

.PHONY: all pdf pdf-a4 pdf-publish epub html docker docker-build clean serve

all: pdf-a4 html

pdf: pdf-a4

pdf-a4: beam-book-a4.pdf

pdf-publish: beam-book-publish.pdf

chapters/contributors.txt:
	{ \
	echo '[cols="3*",frame=none,grid=none]'; \
	echo '|==='; \
	{ \
	git --no-pager log | git --no-pager shortlog -s -n \
	| awk '{$$1=""; sub(/^ /, ""); print}' \
	| grep -Ev "happi|Erik Stenman|Your Name"; \
	if command -v gh >/dev/null 2>&1; then \
		{ \
		gh issue list --state all --json author 2>/dev/null | grep -o '"login":"[^"]*"' | cut -d'"' -f4; \
		gh pr list --state all --json author 2>/dev/null | grep -o '"login":"[^"]*"' | cut -d'"' -f4; \
		} | sort -u | grep -v happi; \
	fi; \
	} | sort -u \
	| paste - - - | sed 's/^\(.*\)\t\(.*\)\t\(.*\)$$/| \1 | \2 | \3/' \
	| sed 's/\t/|/g' \
	| sed 's/| */| /g'; \
	echo '|==='; \
	} > $@

## A4 screen/PDF reading (Default)
beam-book-a4.pdf: style/pdf-online-theme.yml style/pdf-theme.yml chapters/opcodes_doc.asciidoc online-book.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS)
	bundle exec asciidoctor-pdf -r asciidoctor-diagram \
	-r ./style/custom-pdf-converter.rb \
	-r ./style/custom-admonition-block.rb \
	-a config=./style/ditaa.cfg \
	-a pdf-fontsdir=./style/fonts \
	-a source-highlighter=rouge \
	-a rouge-style=pastie \
	-a rouge-linenums-mode=table \
	-a version=$(VERSION) \
 	online-book.asciidoc -o $@

# Print-ready 6×9 for PoD
pub: beam-book-publish.pdf

beam-book-publish.pdf: style/custom-print-highlight-theme.yml style/pdf-publish-theme.yml chapters/opcodes_doc.asciidoc print-book.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS) style/pdf-theme.yml
	bundle exec asciidoctor-pdf -r asciidoctor-diagram \
	-r ./style/custom-pdf-converter.rb \
	-r ./style/custom-admonition-block.rb \
	-a config=./style/ditaa.cfg \
	-a pdf-fontsdir=./style/fonts \
	-a source-highlighter=rouge \
	-a rouge-style=pastie \
	-a rouge-linenums-mode=table \
	-a version=$(VERSION) \
 	print-book.asciidoc -o $@ --trace

# EPUB version
epub: beam-book.epub

beam-book.epub: chapters/opcodes_doc.asciidoc epub-book.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS)
	bundle exec asciidoctor-epub3 -r asciidoctor-diagram \
	-r ./style/custom-admonition-block.rb \
	-a config=./style/ditaa.cfg \
	-a source-highlighter=rouge \
	-a rouge-style=pastie \
	-a version=$(VERSION) \
	epub-book.asciidoc -o $@ --trace


html: chapters/contributors.txt $(ASSET_CHAPTERS)
	cp -r images site
	asciidoctor -r asciidoctor-diagram -r ./style/custom-admonition-block.rb \
	  -a config=style/ditaa.cfg --backend=html5 --doctype=book \
	  -o site/index.html online-book.asciidoc --trace \
	  -a source-highlighter=rouge \
	  -a rouge-style=thankful_eyes \
	  -a rouge-linenums-mode=table \
	  -a version=$(VERSION)
	rsync -R code/*/*.png site

code/book/ebin/generate_op_doc.beam: code/book/src/generate_op_doc.erl
	erlc -o $(dir $@) $<

chapters/opcodes_doc.asciidoc: genop.tab code/book/ebin/generate_op_doc.beam
	erl -pa code/book/ebin/ -noshell -s generate_op_doc from_shell genop.tab chapters/opcodes_doc.asciidoc

genop.tab:
	wget -O genop.tab https://raw.githubusercontent.com/erlang/otp/master/lib/compiler/src/genop.tab || curl -o genop.tab https://raw.githubusercontent.com/erlang/otp/master/lib/compiler/src/genop.tab
	touch $@

clean:
	find site -type f -name '.[^gitignore]*' -delete
	rm -f beam-book-*.pdf beam-book*.epub site/index.html site/*.png site/*.md5 xml/*.png xml/*.md5 xml/beam-book-from-ab.xml images/diag-*.png
	rm -rf site/code site/images .asciidoctor site/.asciidoctor

serve: all
	cd site && python3 -m http.server

docker:
	docker run -v .:/documents thebeambook/builder

docker-build:
	docker build --tag thebeambook/builder - < ./docker/Dockerfile
