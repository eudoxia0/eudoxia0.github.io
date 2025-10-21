.PHONY: all

all: words_per_month.png cumulative_words_per_month.png

POSTS := article/_posts/*.md

words_per_month.png: $(POSTS) stats.py
	python3 stats.py words_per_month

cumulative_words_per_month.png: $(POSTS) stats.py
	python3 stats.py cumulative_words_per_month

.PHONY: clean
clean:
	rm -f words_per_month.png
	rm -f cumulative_words_per_month.png
