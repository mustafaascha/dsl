
CHAPTERS := pattern-matching-dsl.txt
MD_CHAPTERS := $(CHAPTERS:.txt=.md)

PANDOC := pandoc
PANDOC_OPTS_ALL :=  --standalone \
								--variable secnumdepth=0 \
								-f markdown+smart
PANDOC_DOCX_OPTS := $(PANDOC_OPTS_ALL) \
								--default-image-extension=pdf \
								--variable links-as-notes

pattern-matching-dsl.docx: $(MD_CHAPTERS) Makefile
	$(PANDOC) $(PANDOC_DOCX_OPTS) -o $@ $(MD_CHAPTERS)

%.md: %.Rmd
		../runknitr.sh $<

%.Rmd :: %.txt knitr_header.Rmd
		cat knitr_header.Rmd > $@
		cat $< >> $@
