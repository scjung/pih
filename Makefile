MASTER = master.tex
PART   = ch01.tex

PDF = $(MASTER:.tex=.pdf)
TO_CLEAN = $(MASTER:.tex=.aux) $(MASTER:.tex=.log) $(MASTER:.tex=.toc) \
           $(MASTER:.tex=.out) $(MASTER:.tex=.pdf) $(MASTER:.tex=.pdfsync)

%.pdf: %.tex
	pdflatex $<

all: res $(PART) $(PDF)
	pdflatex $(MASTER)

res:
	make -C res all

clean:
	make -C res clean
	rm -f $(TO_CLEAN)

.PHONY: all clean res
