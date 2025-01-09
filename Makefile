ASSET_CHAPTERS = $(shell find chapters -type f)

.PHONY: all pdf html docker docker-build clean serve

all: pdf html

pdf: beam-book.pdf

chapters/contributors.txt: .git
	git --no-pager log | git --no-pager shortlog -s -n | awk '{$$1=""}1' | grep -v "Your Name" > $@

beam-book.pdf:  chapters/opcodes_doc.asciidoc book.asciidoc chapters/contributors.txt $(ASSET_CHAPTERS)
	asciidoctor-pdf  -r ./style/custom-pdf-converter.rb -r asciidoctor-diagram -r ./style/custom-admonition-block.rb  -a config=./style/ditaa.cfg --doctype=book -a pdf-style=./style/pdf-theme.yml book.asciidoc -o $@

html: chapters/contributors.txt $(ASSET_CHAPTERS)
	cp -r images site
	asciidoctor -r asciidoctor-diagram  -r ./style/custom-admonition-block.rb -a config=style/ditaa.cfg --backend=html5 --doctype=book -o site/index.html book.asciidoc --trace
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
	rm -f beam-book.pdf site/index.html site/*.png site/*.md5 xml/*.png xml/*.md5 xml/beam-book-from-ab.xml images/diag-*.png
	rm -rf site/code site/images .asciidoctor site/.asciidoctor

serve: all
	cd site && python3 -m http.server

docker:
	docker run -v .:/documents thebeambook/builder

docker-build:
	docker build --tag thebeambook/builder - < ./docker/Dockerfile
