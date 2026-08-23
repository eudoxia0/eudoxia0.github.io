all: words.png cumulative_words.png posts.png cumulative_posts.png

words.png:
	uv run stats.py words

cumulative_words.png:
	uv run stats.py cumulative_words

posts.png:
	uv run stats.py posts

cumulative_posts.png:
	uv run stats.py cumulative_posts

clean:
	rm -f words.png
	rm -f cumulative_words.png
	rm -f posts.png
	rm -f cumulative_posts.png
