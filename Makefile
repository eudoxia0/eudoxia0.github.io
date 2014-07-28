default: all

STATIC = static
BUILD = build

# Stylesheets
STYLE = $(STATIC)/style.scss
SASS_OPTS = --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = $(STATIC)/style.css

# Command to process an XML file
MAIN_STYLESHEET = transform.xsl
PARAGRAPH_STYLESHEET = final.xsl
XSLT = saxon-xslt

# Targets
XML_FILES = $(shell find . -type f -name '*.xml')
HTML_FILES = $(XML_FILES:.xml=.html)

DOT_FILES = $(shell find . -type f -name '*.dot')
SVG_FILES = $(DOT_FILES:.dot=.svg)

# CV
CV_TEX = cv/cv.tex
CV = cv/cv.pdf

# Moving files
OUTPUT_EXTS = html pdf css js svg png jpg woff
EXTENSIONS = $(foreach EXT, $(OUTPUT_EXTS), --include "*.$(EXT)")
RSYNC_OPTS = -a --exclude "$(BUILD)" --include '*/' $(EXTENSIONS) --exclude '*' \
	--prune-empty-dirs

GI = gem install --no-ri --no-rdoc

reqs:
	$(GI) sass
	$(GI) bourbon
	$(GI) neat
	bower install
	cd $(STATIC); bourbon install
	cd $(STATIC); neat install

%.html: %.xml
	$(XSLT) $? $(MAIN_STYLESHEET) > tmp.xml
	$(XSLT) tmp.xml $(PARAGRAPH_STYLESHEET) > $@
	rm tmp.xml

%.svg: %.dot
	dot -Tsvg $? -o $@

$(CV): $(CV_TEX)
	pdflatex $?
	rm cv.log
	rm cv.aux
	rm cv.out
	mv cv.pdf $@

$(TARGET_CSS): $(STYLE)
	$(SASS) $(STYLE) $(TARGET_CSS)

all: $(STATIC) $(TARGET_CSS) $(HTML_FILES) $(SVG_FILES) $(CV)
	mkdir -p $(BUILD)
	rsync $(RSYNC_OPTS) ./ $(BUILD)

serve:
	cd $(BUILD); python2 -m SimpleHTTPServer 5000

clean:
	rm -rf $(BUILD)
	find . -type f -name '*.html' -delete
	find . -type f -name '*.css' -delete
	find . -type f -name '*.pdf' -delete
