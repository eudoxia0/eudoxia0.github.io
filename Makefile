.PHONY: all

all: words_per_month.png \
	cumulative_words_per_month.png \
	assets/content/design-austral-compiler/pipeline.png

assets/content/design-austral-compiler/pipeline.png: assets/content/design-austral-compiler/pipeline.dot
	dot -T png assets/content/design-austral-compiler/pipeline.dot -o assets/content/design-austral-compiler/pipeline.png

POSTS := article/_posts/*.md

words_per_month.png: $(POSTS) stats.py
	python stats.py words_per_month

cumulative_words_per_month.png: $(POSTS) stats.py
	python stats.py cumulative_words_per_month

.PHONY: clean
clean:
	rm -f assets/content/design-austral-compiler/pipeline.png
	rm -f words_per_month.png
	rm -f cumulative_words_per_month.png
