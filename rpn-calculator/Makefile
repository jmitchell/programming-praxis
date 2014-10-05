all: main.native

test: test.ml rpn.ml
	corebuild test.native && ./test.native

clean:
	rm -rf _build
	rm *.native

main.native: main.ml rpn.ml
	corebuild main.native
