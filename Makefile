LISPCORE = sbcl.core # For faster loading
LISP = sbcl --core $(LISPCORE) --script

BUILD = build

default: all

$(LISPCORE):
	sbcl --eval '(sb-ext:save-lisp-and-die "sbcl.core")' --quit

all: $(LISPCORE)
	mkdir -p $(BUILD)
	$(LISP) build.lisp
