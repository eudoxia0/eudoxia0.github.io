.PHONY: all clean

DOT_FILES := $(wildcard *.dot)
SVG_FILES := $(DOT_FILES:.dot=.svg)

all: $(SVG_FILES)

%.svg: %.dot
	dot -T svg $< -o $@

clean:
	rm -f $(SVG_FILES)
