LISPCORE = sbcl.core # For faster loading
LISP = sbcl --core $(LISPCORE) --noinform

GEM_REQS = sass bourbon neat

STYLE = static/css/style.scss
SASS_OPTS = -I static -I static/css --style compressed
SASS = sass $(SASS_OPTS)
TARGET_CSS = build/static/css/style.css

BUILD = build

default: all

$(LISPCORE):
	@echo "Building core"
	sbcl --noinform --eval '(sb-ext:save-lisp-and-die "sbcl.core")' \
	     --quit

$(BUILD):
	mkdir -p $(BUILD)/static/css
	mkdir $(BUILD)/static/js

reqs:
	$(foreach GEM, $(GEM_REQS), gem install $(GEM);)
	bower install
	cd static; bourbon install; neat install

$(TARGET_CSS): $(STYLE)
	$(SASS) $? $@

all: $(LISPCORE) $(BUILD) $(TARGET_CSS)
	$(LISP) --load lib/markup.lisp --load site.lisp --quit

clean: $(BUILD)
	rm -rf build/

serve: all
	cd build; python2 -m SimpleHTTPServer 5000

.PHONY: reqs clean serve
