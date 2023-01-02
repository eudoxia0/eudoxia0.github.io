.PHONY: all
all: epiphany.html

style.css: style.scss
	scss style.scss > style.css

compiled.md:
	python3 copy.py

epiphany.html: compiled.md template.html style.css
	pandoc -s --toc --template=template.html --toc-depth=1 --section-divs --reference-location=section compiled.md -o epiphany.html

clean:
	rm compiled.md
	rm style.css
	rm epiphany.html
