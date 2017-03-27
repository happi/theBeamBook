
ABFLAGS = --backend=docbook --doctype=book --attribute=revisionhistory
adocs = book.asciidoc \
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
        type_system.asciidoc \
        ap-beam_instructions.asciidoc \
        ap-code_listings.asciidoc
        #     beam.asciidoc \
        #     erts-book.asciidoc  \


all: beam-book.pdf

book-revhistory.xml: .git hg2revhistory.xsl
	./gitlog.sh git-log.xml $@ 

beam-book-from-ab.xml:  $(adocs)\
                       book-revhistory.xml
	asciidoc $(ABFLAGS) -o $@ book.asciidoc

beam-book.pdf: beam-book-from-ab.xml book-revhistory.xml
	dblatex beam-book-from-ab.xml -o $@ 


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
