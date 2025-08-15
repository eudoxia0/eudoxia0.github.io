all: diff.png newman.png strong.png

diff.png: diff.pdf
	magick -density 300 diff.pdf diff.png

newman.png: newman.pdf
	magick -density 300 newman.pdf newman.png

strong.png: strong.pdf
	magick -density 300 strong.pdf strong.png

diff.pdf: diff.tex
	pdflatex diff.tex
	rm diff.aux
	rm diff.log

newman.pdf: newman.tex
	pdflatex newman.tex
	rm newman.aux
	rm newman.log

strong.pdf: strong.tex
	pdflatex strong.tex
	rm strong.aux
	rm strong.log

.PHONY: clean
clean:
	rm -f diff.png
	rm -f diff.pdf
	rm -f newman.png
	rm -f newman.pdf
	rm -f strong.png
	rm -f strong.pdf
