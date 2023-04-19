CODE := solver.ml
FRAG := frags.json
TMPL := template.md
BLOG := ../../../article/_posts/2023-04-20-writing-sat-solver-ocaml.md
TEXT := output.txt

.PHONY: all clean
all: $(BLOG)
	
$(TEXT): $(CODE)
	ocaml $(CODE) > $(TEXT)

$(FRAG): $(CODE) $(TEXT)
	airloom lift $(CODE) $(TEXT) -o $(FRAG)

$(BLOG): $(FRAG) $(TMPL)
	airloom weave $(TMPL) -f $(FRAG) -o $(BLOG)
	
clean:
	rm -f $(FRAG)
	rm -f $(TEXT)
	rm -f $(BLOG)