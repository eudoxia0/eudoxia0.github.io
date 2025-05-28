all: diff.png

diff.png: diff.pdf
	magick -density 300 diff.pdf diff.png

diff.pdf: diff.tex
	pdflatex diff.tex
	rm diff.aux
	rm diff.log

.PHONY: clean
clean:
	rm -f diff.png
	rm -f diff.pdf
