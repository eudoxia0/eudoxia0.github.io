all: diff.png newman.png

diff.png: diff.pdf
	magick -density 300 diff.pdf diff.png

newman.png: newman.pdf
	magick -density 300 newman.pdf newman.png

diff.pdf: diff.tex
	pdflatex diff.tex
	rm diff.aux
	rm diff.log

newman.pdf: newman.tex
	pdflatex newman.tex
	rm newman.aux
	rm newman.log

.PHONY: clean
clean:
	rm -f diff.png
	rm -f diff.pdf
