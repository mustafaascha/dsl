
PANDOC := pandoc

PANDOC_OPTS_ALL :=  --standalone --toc \
					-f markdown+smart \
					--top-level-division=part \
					--filter pandoc-crossref
PANDOC_PDF_OPTS := $(PANDOC_OPTS_ALL) \
					--default-image-extension=pdf \
					--variable links-as-notes \
					--template=../templates/latex-template.tex

PANDOC_PRINT_OPTS := $(PANDOC_PDF_OPTS) --no-highlight

PDF_CHAPTERS := $(foreach chapter,$(CHAPTERS),$(chapter))

book.pdf: $(PDF_CHAPTERS) Makefile ../Makefile ../templates/latex-template.tex
	$(PANDOC) $(PANDOC_PDF_OPTS) -o $@ $(PDF_CHAPTERS)

%.md: %.Rmd
	../runknitr.sh $<

%.Rmd :: ../chapters/%.md
	cat ../chapters/000_knitr_header.Rmd | gpp -DPDF > $@
	cat $< | gpp -DPDF >> $@
