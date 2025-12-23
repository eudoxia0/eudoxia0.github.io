EPUB := ../assets/content/eog581/The\ Epiphany\ of\ Gliese\ 581.epub
PDF  := ../assets/content/eog581/The\ Epiphany\ of\ Gliese\ 581.pdf

.PHONY: all
all: $(EPUB) $(PDF)

$(EPUB): compiled.md
	pandoc -s compiled.md -o $(EPUB)

compiled.md: copy.py
	python copy.py

$(PDF): epiphany.html
	chromium --headless \
	    --run-all-compositor-stages-before-draw \
		--print-to-pdf="$@" \
		--no-pdf-header-footer \
		$<

epiphany.html: compiled.md template.html style.css
	pandoc --standalone --template=template.html --section-divs --reference-location=section compiled.md -o epiphany.html

style.css: style.scss
	sass style.scss > style.css

.PHONY: clean
clean:
	rm -f compiled.md style.css epiphany.html $(EPUB) $(PDF)
