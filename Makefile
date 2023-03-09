assets/content/design-austral-compiler/pipeline.png: assets/content/design-austral-compiler/pipeline.dot
	dot -T png assets/content/design-austral-compiler/pipeline.dot -o assets/content/design-austral-compiler/pipeline.png
	
.PHONY: clean
clean:
	rm assets/content/design-austral-compiler/pipeline.png