all: run

lint:
	indent -kr -ts4 -brf *.c

clean:
	rm -rf a.out

compile: clean lint
	gcc -Wall -Wextra *.c

run: compile
	./a.out

valgrind: compile
	valgrind -s --leak-check=full ./a.out
