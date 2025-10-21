all: pipeline.png

pipeline.png: pipeline.dot
	dot -T png pipeline.dot -o pipeline.png

clean:
	rm -f pipeline.png
