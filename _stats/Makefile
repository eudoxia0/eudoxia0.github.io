all: words_per_month.png cumulative_words_per_month.png

words_per_month.png:
	uv run stats.py words_per_month

cumulative_words_per_month.png:
	uv run stats.py cumulative_words_per_month

clean:
	rm -f words_per_month.png
	rm -f cumulative_words_per_month.png
