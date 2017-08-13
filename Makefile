
CHAPTERS := 000_header.md \
			01_Introduction.md \
			02_ADT_and_complexity.md \
			03_Immutable_and_persistent.md \
			04_Bags_stacks_and_queues.md \
			05_Heaps.md \
			06_Sets_and_search_trees.md \
			xx_conclusions.md


SOURCE_CHAPTERS := $(foreach chapter,$(CHAPTERS),chapters/$(chapter))


book.pdf: $(SOURCE_CHAPTERS) Makefile
	(cd pdf_book && make CHAPTERS="$(CHAPTERS)")
	cp pdf_book/book.pdf book.pdf

print_book.pdf: $(SOURCE_CHAPTERS) Makefile pdf_book/Makefile
	(cd pdf_book && make print_book.pdf CHAPTERS="$(CHAPTERS)")
	cp pdf_book/print_book.pdf print_book.pdf

book.epub:  $(SOURCE_CHAPTERS) Makefile
	(cd ebook && make CHAPTERS="$(CHAPTERS)")
	cp ebook/book.epub book.epub

all: book.pdf book.epub

book.mobi: book.epub
	./kindlegen book.epub -o book.mobi

all: book.pdf book.epub book.mobi print_book.pdf

clean:
	rm book.pdf book.epub book.mobi
