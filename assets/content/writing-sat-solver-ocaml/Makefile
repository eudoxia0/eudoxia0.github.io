CODE := solver.ml
FRAG := frags.json
TMPL := template.md
BLOG := ../../../article/_posts/2023-04-20-writing-sat-solver-ocaml.md
TEXT := output.txt
BRUTEFORCE_DOT := bruteforcing.dot
BRUTEFORCE_SVG := bruteforcing.svg

.PHONY: all clean
all: $(BLOG)
	
$(TEXT): $(CODE)
	ocaml $(CODE) > $(TEXT)

$(FRAG): $(CODE) $(TEXT)
	airloom lift $(CODE) $(TEXT) -o $(FRAG)

$(BLOG): $(FRAG) $(TMPL) $(BRUTEFORCE_SVG)
	airloom weave $(TMPL) -f $(FRAG) -o $(BLOG)

$(BRUTEFORCE_SVG): $(BRUTEFORCE_DOT)
	dot -Tsvg $(BRUTEFORCE_DOT) -o $(BRUTEFORCE_SVG)
	
clean:
	rm -f $(FRAG)
	rm -f $(TEXT)
	rm -f $(BLOG)
	rm -f $(BRUTEFORCE_SVG)