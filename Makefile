
ABFLAGS = --backend=docbook --doctype=book --attribute=revisionhistory
adocs = book.asciidoc \
        beam.asciidoc \
        beam_instructions.asciidoc \
	beam_internal_instructions.asciidoc \
	beam_modules.asciidoc \
	calls.asciidoc \
        compiler.asciidoc \
        introduction.asciidoc \
        memory.asciidoc \
	opcodes_doc.asciidoc \
	preface.asciidoc \
	processes.asciidoc \
	scheduling.asciidoc \
        type_system.asciidoc \
        ap-beam_instructions.asciidoc \
        ap-code_listings.asciidoc \
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

book-revhistory.xml: .git hg2revhistory.xsl
	./gitlog.sh git-log.xml $@ 

beam-book-from-ab.xml:  $(adocs)\
                       book-revhistory.xml
	asciidoc $(ABFLAGS) -o $@ book.asciidoc

beam-book.pdf: beam-book-from-ab.xml book-revhistory.xml
	dblatex beam-book-from-ab.xml -o $@ 

book.html:
	asciidoc --backend=html5 --doctype=book book.asciidoc

# $(subst .asciidoc,.html, $(adocs)): %.html: %.asciidoc 
# 	asciidoc -a icons -a toc2 $<

# index.html: *.asciidoc
# 	asciidoc -o index.html -a icons -a toc2 book.asciidoc 

code/book/ebin/generate_op_doc.beam: code/book/src/generate_op_doc.erl
	erlc -o $@ $<

opcodes_doc.asciidoc: genop.tab code/book/ebin/generate_op_doc.beam
	erl -noshell -s generate_op_doc from_shell genop.tab opcodes_doc.asciidoc


# generate_op_doc.beam: generate_op_doc.erl
# 	erlc generate_op_doc.erl

# opcodes_doc.asciidoc: ../otp/lib/compiler/src/genop.tab generate_op_doc.beam
# 	erl -noshell -s generate_op_doc from_shell ../otp/lib/compiler/src/genop.tab opcodes_doc.asciidoc
