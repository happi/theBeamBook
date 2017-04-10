
ABFLAGS = --backend=docbook --doctype=book
adocs = book.asciidoc \
        chapters/beam.asciidoc \
        chapters/beam_instructions.asciidoc \
	chapters/beam_internal_instructions.asciidoc \
	chapters/beam_modules.asciidoc \
	chapters/building.asciidoc \
	chapters/c.asciidoc \
	chapters/calls.asciidoc \
        chapters/compiler.asciidoc \
	chapters/contributors.txt \
        chapters/introduction.asciidoc \
        chapters/memory.asciidoc \
	chapters/opcodes_doc.asciidoc \
	chapters/preface.asciidoc \
	chapters/processes.asciidoc \
	chapters/scheduling.asciidoc \
        chapters/type_system.asciidoc \
        chapters/ap-beam_instructions.asciidoc \
        chapters/ap-code_listings.asciidoc \
	code/beam_modules_chapter/src/beamfile.erl \
        code/compiler_chapter/json_tokens.png \
        code/compiler_chapter/src/json_parser.erl \
        code/compiler_chapter/src/json_test.erl \
        code/compiler_chapter/src/json_tokens.xrl \
        code/compiler_chapter/src/test.json \
	code/compiler_chapter/src/world.erl \
	code/memory_chapter/src/lb.erl \
	code/memory_chapter/src/send.erl \
	code/memory_chapter/src/share.erl


all: beam-book.pdf book.html

chapters/contributors.txt: .git
	./bin/gitlog.sh $@ 

xml/beam-book-from-ab.xml:  $(adocs)
	asciidoc $(ABFLAGS) -o $@ book.asciidoc

beam-book.pdf: xml/beam-book-from-ab.xml
	dblatex xml/beam-book-from-ab.xml -o $@ 

book.html:
	asciidoc --backend=html5 --doctype=book book.asciidoc

# $(subst .asciidoc,.html, $(adocs)): %.html: %.asciidoc 
# 	asciidoc -a icons -a toc2 $<

# index.html: *.asciidoc
# 	asciidoc -o index.html -a icons -a toc2 book.asciidoc 

code/book/ebin/generate_op_doc.beam: code/book/src/generate_op_doc.erl
	erlc -o $(dir $@) $<

chapters/opcodes_doc.asciidoc: genop.tab code/book/ebin/generate_op_doc.beam
	erl -pa code/book/ebin/ -noshell -s generate_op_doc from_shell genop.tab chapters/opcodes_doc.asciidoc

genop.tab:
	wget -O genop.tab https://raw.githubusercontent.com/erlang/otp/master/lib/compiler/src/genop.tab
	touch $@
