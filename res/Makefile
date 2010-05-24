DOTS = $(wildcard *.dot)
PDFS = $(DOTS:.dot=.pdf)

%.pdf: %.dot
	dot -Tpdf $< -o $@

all: $(PDFS)

clean:
	rm -rf $(PDFS)

.PHONY: all clean