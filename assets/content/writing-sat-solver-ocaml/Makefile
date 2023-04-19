CODE := solver.ml
FRAG := frags.json
TMPL := template.md
BLOG := ../../../article/_posts/2023-04-20-writing-sat-solver-ocaml.md

.PHONY: all clean
all: $(BLOG)

$(FRAG): $(CODE)
	airloom lift $(CODE) -o $(FRAG)

$(BLOG): $(FRAG) $(TMPL)
	airloom weave $(TMPL) -f $(FRAG) -o $(BLOG)
	
clean:
	rm -f $(FRAG)
	rm -f $(BLOG)