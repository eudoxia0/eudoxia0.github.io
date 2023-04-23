CODE := solver.py
FRAG := frags.json
TMPL := template.md
BLOG := ../../../article/_posts/2023-04-23-dependency-resolution-made-simple.md

.PHONY: all clean
all: $(BLOG)

$(FRAG): $(CODE)
	airloom lift $(CODE) -o $(FRAG)

$(BLOG): $(FRAG) $(TMPL)
	airloom weave $(TMPL) -f $(FRAG) -o $(BLOG)

clean:
	rm -f $(FRAG)
	rm -f $(BLOG)
