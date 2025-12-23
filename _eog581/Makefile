EPUB := ../assets/content/eog581/The\ Epiphany\ of\ Gliese\ 581.epub

.PHONY: all
all: style.css epiphany.html $(EPUB)

style.css: style.scss
	sass style.scss > style.css

compiled.md: copy.py
	python copy.py

epiphany.html: compiled.md template.html style.css
	pandoc -s --toc --template=template.html --toc-depth=1 --section-divs --reference-location=section compiled.md -o epiphany.html

$(EPUB): compiled.md
	pandoc -s compiled.md -o $(EPUB)

clean:
	rm compiled.md
	rm style.css
	rm epiphany.html
	rm $(EPUB)
