LISPCORE = sbcl.core # For faster loading
LISP = sbcl --core $(LISPCORE) --noinform

BUILD = build

default: all

$(LISPCORE):
	@echo "Building core"
	sbcl --noinform --eval '(sb-ext:save-lisp-and-die "sbcl.core")' \
	     --quit

all: $(LISPCORE)
	mkdir -p $(BUILD)
	$(LISP) --load lib/markup.lisp --load site.lisp --quit

clean: $(LISPCORE)
	rm $(LISPCORE)
	rm -rf build/
