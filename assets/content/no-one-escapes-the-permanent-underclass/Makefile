pyramid.png: pyramid.pdf
	pdftocairo -png -singlefile -transp -r 300 pyramid.pdf

pyramid.pdf: pyramid.tex
	pdflatex -interaction=nonstopmode -halt-on-error pyramid.tex

clean:
	rm -f *.pdf *.aux *.log *.png
