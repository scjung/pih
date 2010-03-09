DOTS = $(wildcard *.dot)
PNGS = $(DOTS:.dot=.png)

%.png: %.dot
	dot -Tpng $< -o $@

all: $(PNGS)

clean:
	rm -rf $(PNGS)

.PHONY: all clean