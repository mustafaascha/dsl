
CHAPTERS := 000_header.txt \
			Introduction.txt \
			\
			Matrix_expressions.txt \
			Components_of_a_language.txt \
			\
			Functions_expressions_operators.txt \
			Parsing_and_manipulating_expressions.txt \
			Lambda.txt \
			\
			Environments_and_expressions.txt \
			Tidy_eval.txt \
			List_comprehension.txt \
			CTMCs.txt \
			Pattern_matching.txt \
			Dynamic_programming.txt \
			\
			xx_conclusions.txt

# while editing...
CHAPTERS := 000_header.txt \
Introduction.txt


SOURCE_CHAPTERS := $(foreach chapter,$(CHAPTERS),chapters/$(chapter))

all: book.pdf

book.pdf: $(SOURCE_CHAPTERS)  chapters/000_knitr_header.Rmd Makefile pdf_book/Makefile templates/latex-template.tex
	(cd pdf_book && make CHAPTERS="$(CHAPTERS)")
	cp pdf_book/book.pdf book.pdf

book.epub:  $(SOURCE_CHAPTERS) chapters/000_knitr_header.Rmd Makefile ebook/Makefile
	(cd ebook && make CHAPTERS="$(CHAPTERS)")
	cp ebook/book.epub book.epub

#book.mobi: book.epub
#	./kindlegen book.epub -o book.mobi

book.docx: $(SOURCE_CHAPTERS) chapters/000_knitr_header.Rmd Makefile pdf_book/Makefile templates/latex-template.tex
	(cd pdf_book && make book.docx CHAPTERS="$(CHAPTERS)")
	cp pdf_book/book.docx book.docx

clean:
	rm book.pdf book.epub book.docx
