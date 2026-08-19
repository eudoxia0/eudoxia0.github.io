all: graph1.png graph2.png

graph1.png: graph.py
	nix-shell --run "python graph.py sigmoid"

graph2.png: graph.py
	nix-shell --run "python graph.py takeoff"

clean:
	rm -f graph1.png graph2.png
