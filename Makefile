LISPCORE = sbcl.core # For faster loading
LISP = sbcl --core $(LISPCORE) --noinform

GEM_REQS = sass bourbon neat

BUILD = build

default: all

$(LISPCORE):
	@echo "Building core"
	sbcl --noinform --eval '(sb-ext:save-lisp-and-die "sbcl.core")' \
	     --quit

$(BUILD):
	mkdir -p $(BUILD)

reqs:
	$(foreach GEM, $(GEM_REQS), gem install $(GEM);)
	cd static; bourbon install; neat install

all: $(LISPCORE) $(BUILD)
	$(LISP) --load lib/markup.lisp --load site.lisp --quit

clean: $(LISPCORE)
	rm $(LISPCORE)
	rm -rf build/

.PHONY: reqs clean
