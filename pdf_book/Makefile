
PANDOC := pandoc

PANDOC_OPTS_ALL :=  -S --toc \
					--top-level-division=chapter \
					--filter pandoc-crossref \
					--filter pandoc-citeproc
PANDOC_PDF_OPTS := $(PANDOC_OPTS_ALL) \
					--default-image-extension=pdf \
					--variable links-as-notes \
					--template=../templates/latex-template.tex

PANDOC_PRINT_OPTS := $(PANDOC_PDF_OPTS) --no-highlight

PDF_CHAPTERS := $(foreach chapter,$(CHAPTERS),$(chapter))

book.pdf: $(PDF_CHAPTERS)
	$(PANDOC) $(PANDOC_PDF_OPTS) -o $@ $(PDF_CHAPTERS)

print_book.pdf: $(PDF_CHAPTERS)
	$(PANDOC) $(PANDOC_PRINT_OPTS) -o $@ $(PDF_CHAPTERS)

%.md: %.Rmd
	../runknitr.sh $<

%.Rmd :: ../chapters/%.md
	cat ../chapters/000_knitr_header.Rmd | gpp -DPDF > $@
	cat $< | gpp -DPDF >> $@
