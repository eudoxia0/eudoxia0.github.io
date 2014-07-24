default: all

STATIC = static
BUILD = build

# Stylesheets
STYLE = $(STATIC)/style.scss
SASS_OPTS = --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = content/style.css

# Command to process an XML file
STYLESHEET = transform.xsl
XSLT = saxon-xslt

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

XML_FILES = $(shell find content/ -type f -name '*.xml')
HTML_FILES = $(XML_FILES:.xml=.html)

%.html: %.xml
	$(XSLT) $? $(STYLESHEET) > $@

$(TARGET_CSS): $(STYLE)
	$(SASS) $(STYLE) $(TARGET_CSS)

all: $(STATIC) $(TARGET_CSS) $(HTML_FILES)
	mkdir -p $(BUILD)
	find content/ -type f -name "*.html" -exec mv {} $(BUILD) \;


serve: all
	cd $(BUILD); python2 -m SimpleHTTPServer 5000

clean:
	rm -rf $(BUILD)
