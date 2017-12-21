
CHAPTERS := 000_header.md \
			Introduction.md \
			\
			Matrix_expressions.md \
			Components_of_a_language.md \
			\
			Functions_expressions_operators.md \
			Parsing_and_manipulating_expressions.md \
			\
			Environments_and_expressions.md \
			List_comprehension_and_pattern_matching.md \
			\
			xx_conclusions.md


SOURCE_CHAPTERS := $(foreach chapter,$(CHAPTERS),chapters/$(chapter))

all: book.pdf book.epub book.mobi

book.pdf: $(SOURCE_CHAPTERS) Makefile pdf_book/Makefile templates/latex-template.tex
	(cd pdf_book && make CHAPTERS="$(CHAPTERS)")
	cp pdf_book/book.pdf book.pdf

book.epub:  $(SOURCE_CHAPTERS) Makefile ebook/Makefile
	(cd ebook && make CHAPTERS="$(CHAPTERS)")
	cp ebook/book.epub book.epub

book.mobi: book.epub
	./kindlegen book.epub -o book.mobi

all: book.pdf book.epub book.mobi

clean:
	rm book.pdf book.epub book.mobi
