all: clean compile run

lint:
	indent -kr -ts4 -brf *.c

clean:
	rm -r a.out

compile: 
	gcc *.c

run:
	./a.out
