all: run

lint:
	indent -kr -ts4 -brf *.c

clean:
	rm -rf a.out

compile: clean lint
	gcc *.c

run: compile
	./a.out
