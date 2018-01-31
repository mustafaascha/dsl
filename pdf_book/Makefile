
PANDOC := pandoc

PANDOC_OPTS_ALL :=  --standalone --toc \
					-f markdown+smart \
					--top-level-division=chapter \
					--filter pandoc-crossref \
                    --filter pandoc-citeproc
PANDOC_PDF_OPTS := $(PANDOC_OPTS_ALL) \
					--default-image-extension=pdf \
					--variable links-as-notes \
					--template=../templates/latex-template.tex
PANDOC_DOCX_OPTS := $(PANDOC_OPTS_ALL) \
					--default-image-extension=pdf \
					--variable links-as-notes 

PANDOC_PRINT_OPTS := $(PANDOC_PDF_OPTS) --no-highlight

PDF_CHAPTERS := $(foreach chapter,$(CHAPTERS),$(chapter))

book.pdf: $(PDF_CHAPTERS) Makefile ../Makefile ../templates/latex-template.tex
	$(PANDOC) $(PANDOC_PDF_OPTS) -o $@ $(PDF_CHAPTERS)

book.docx: $(PDF_CHAPTERS) Makefile ../Makefile
	$(PANDOC) $(PANDOC_DOCX_OPTS) -o $@ $(PDF_CHAPTERS)

%.txt: %.Rmd
	../runknitr.sh $<
	mv `basename $@ .txt`.md $@

%.Rmd :: ../chapters/%.txt
	cat ../chapters/000_knitr_header.Rmd | gpp -DPDF > $@
	cat $< | gpp -DPDF >> $@
