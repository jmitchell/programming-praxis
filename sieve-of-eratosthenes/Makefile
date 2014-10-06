all: main.native

test: test.ml sieve.ml
	corebuild test.native && ./test.native

clean:
	rm -rf _build
	rm *.native

main.native: main.ml sieve.ml
	corebuild main.native
