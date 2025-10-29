TOC := _includes/toc.html

all: $(TOC)

$(TOC):
	mkdir -p _includes
	curl -sL -o $(TOC) "https://raw.githubusercontent.com/allejo/jekyll-toc/refs/heads/master/_includes/toc.html"

clean:
	rm -f $(TOC)
