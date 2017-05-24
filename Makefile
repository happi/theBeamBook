ABFLAGS = --backend=docbook --doctype=book

DBLATEX_OPTS = -P latex.output.revhistory=0 -P doc.collab.show=0

all: chapters/contributors.txt beam-book.pdf index.html

chapters/contributors.txt: .git
	./bin/gitlog.sh $@

xml/beam-book-from-ab.xml:  chapters/opcodes_doc.asciidoc
	asciidoc $(ABFLAGS) -o $@ book.asciidoc

beam-book.pdf: xml/beam-book-from-ab.xml
	dblatex $(DBLATEX_OPTS) xml/beam-book-from-ab.xml -o $@

index.html:
	asciidoctor -r asciidoctor-diagram --backend=html5 --doctype=book -a icons=font -a toc2 -o site/index.html book.asciidoc

code/book/ebin/generate_op_doc.beam: code/book/src/generate_op_doc.erl
	erlc -o $(dir $@) $<

chapters/opcodes_doc.asciidoc: genop.tab code/book/ebin/generate_op_doc.beam
	erl -pa code/book/ebin/ -noshell -s generate_op_doc from_shell genop.tab chapters/opcodes_doc.asciidoc

genop.tab:
	wget -O genop.tab https://raw.githubusercontent.com/erlang/otp/master/lib/compiler/src/genop.tab
	touch $@

clean:
	rm -f beam-book.pdf site/index.html site/*.png site/*.md5 xml/*.png xml/*.md5 xml/beam-book-from-ab.xml
