
PANDOC := pandoc

PANDOC_OPTS_ALL :=  --standalone --toc \
					-f markdown+smart \
					--top-level-division=chapter \
					--filter pandoc-crossref \
                    --filter pandoc-citeproc
PANDOC_EPUB_OPTS := $(PANDOC_OPTS_ALL) \
					--default-image-extension=png \
					-t epub3 --toc-depth=1 \
					--epub-cover-image=../cover.jpg

EPUB_CHAPTERS := $(foreach chapter,$(CHAPTERS),$(chapter))

book.epub: $(EPUB_CHAPTERS) Makefile ../Makefile
	$(PANDOC) $(PANDOC_EPUB_OPTS) -o $@ $(EPUB_CHAPTERS)


%.txt: %.Rmd
	../runknitr.sh $<
	mv `basename $@ .txt`.md $@

%.Rmd :: ../chapters/%.txt
	cat ../chapters/000_knitr_header.Rmd | gpp -DEPUB > $@
	cat $< | gpp -DEPUB >> $@
