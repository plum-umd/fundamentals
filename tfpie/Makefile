CURRENT=tfpie

WGETDVANHORNBIB=curl -o dvanhorn.bib "http://www.citeulike.org/bibtex/user/dvanhorn?fieldmap=posted-at:date-added&do_username_prefix=1&key_type=4&fieldmap=url:x-url&fieldmap=doi:x-doi&fieldmap=address:x-address&fieldmap=isbn:x-isbn&fieldmap=issn:x-issn&fieldmap=month:x-month&fieldmap=comment:comment&fieldmap=booktitle:booktitle&fieldmap=abstract:x-abstract&fieldmap=pages:pages&volume:volume"
WGETSAMTHBIB=curl -o samth.bib "http://www.citeulike.org/bibtex/user/samth?fieldmap=posted-at:date-added&do_username_prefix=1&key_type=4&fieldmap=url:x-url&fieldmap=doi:x-doi&fieldmap=address:x-address&fieldmap=isbn:x-isbn&fieldmap=issn:x-issn&fieldmap=month:x-month&fieldmap=comment:comment&fieldmap=booktitle:booktitle&fieldmap=abstract:x-abstract&fieldmap=pages:pages"

default: 
	rubber -Wall -d $(CURRENT)

.PHONY: wc
wc:
	@wc -w abstract.tex
	@wc -w content.tex

refresh: getbib

getbib:
	$(WGETDVANHORNBIB)
	$(WGETSAMTHBIB)
	-bibclean dvanhorn.bib > dvh-bibliography.bib.clean
	-mv dvh-bibliography.bib.clean dvh-bibliography.bib
	-bibclean samth.bib > sth-bibliography.bib.clean
	-mv sth-bibliography.bib.clean sth-bibliography.bib

bibtex:
	bibtex $(CURRENT)

%.dvi: %.tex 
	latex $(basename $@)

%.pdf: %.tex
	rubber -Wall -d $(basename $@)

#%.pdf: %.dvi
#	dvipdfm -o $(basename $@).pdf $(basename $@).dvi

flush: clean
	rm -f dvanhorn.bib

clean:
	rm -f $(CURRENT).{dvi,ps,pdf,log,toc,blg,bbl,aux,rel} *.log *~ *.out


