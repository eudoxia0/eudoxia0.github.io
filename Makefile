default: all

STYLE = style.scss
SASS_OPTS = --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = content/style.css

STATIC = static

GI = gem install --no-ri --no-rdoc

reqs:
	$(GI) sass
	$(GI) bourbon
	$(GI) neat
	$(GI) nanoc
	$(GI) pandoc-ruby

$(STATIC):
	mkdir -p $(STATIC)
	bower install
	cd $(STATIC); bourbon install
	cd $(STATIC); neat install

$(TARGET_CSS): $(STYLE)
	$(SASS) $(STYLE) $(TARGET_CSS)

all: $(STATIC) $(TARGET_CSS)
	nanoc

serve: all
	cd output; python2 -m SimpleHTTPServer 5000
