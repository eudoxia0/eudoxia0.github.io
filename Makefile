default: all

STATIC = static
BUILD = build

# Stylesheets
STYLE = $(STATIC)/style.scss
SASS_OPTS = --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = $(STATIC)/style.css

# Command to process an XML file
STYLESHEET = transform.xsl
XSLT = saxon-xslt

# Targets
XML_FILES = $(shell find . -type f -name '*.xml')
HTML_FILES = $(XML_FILES:.xml=.html)

# CV
CV_TEX = cv/cv.tex
CV = cv/cv.pdf

# Moving files
OUTPUT_EXTS = html pdf css
EXTENSIONS = $(foreach EXT, $(OUTPUT_EXTS), --include "*.$(EXT)")
RSYNC_OPTS = -a --include '*/' $(EXTENSIONS) --exclude '*' --exclude $(BUILD) \
	--prune-empty-dirs --remove-source-files

GI = gem install --no-ri --no-rdoc

reqs:
	$(GI) sass
	$(GI) bourbon
	$(GI) neat

$(STATIC):
	mkdir -p $(STATIC)
	bower install
	cd $(STATIC); bourbon install
	cd $(STATIC); neat install

%.html: %.xml
	$(XSLT) $? $(STYLESHEET) > $@

$(CV): $(CV_TEX)
	pdflatex $?
	rm cv.log
	rm cv.aux
	rm cv.out
	mv cv.pdf $@

$(TARGET_CSS): $(STYLE)
	$(SASS) $(STYLE) $(TARGET_CSS)

all: $(STATIC) $(TARGET_CSS) $(HTML_FILES) $(CV)
	mkdir -p $(BUILD)
	rsync $(RSYNC_OPTS) ./ $(BUILD)

serve: all
	cd $(BUILD); python2 -m SimpleHTTPServer 5000

clean:
	rm -rf $(BUILD)
