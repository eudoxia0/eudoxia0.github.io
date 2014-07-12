default: all

STYLE = style.scss
SASS_OPTS = --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = content/style.css

all:
	$(SASS) $(STYLE) $(TARGET_CSS)
	nanoc

serve: all
	cd output; python2 -m SimpleHTTPServer 5000
