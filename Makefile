
ABFLAGS = --backend=docbook --doctype=book --attribute=revisionhistory
adocs = book.asciidoc 
                   #     introduction.asciidoc \
                   #     preface.asciidoc \
                   #     compiler.asciidoc \
                   #     beam.asciidoc \
                   #     beam_modules.asciidoc \
                   #     processes.asciidoc \
                   #     type_system.asciidoc \
                   #     memory.asciidoc \
                   #     erts-book.asciidoc  \
                   #     ap-beam_instructions.asciidoc \
	           # opcodes_doc.asciidoc

all: beam-book.pdf

book-revhistory.xml: .git
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

# generate_op_doc.beam: generate_op_doc.erl
# 	erlc generate_op_doc.erl

# opcodes_doc.asciidoc: ../otp/lib/compiler/src/genop.tab generate_op_doc.beam
# 	erl -noshell -s generate_op_doc from_shell ../otp/lib/compiler/src/genop.tab opcodes_doc.asciidoc
