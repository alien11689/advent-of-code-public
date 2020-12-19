
class Tape {
	int cur = 0
	Map values = [:].withDefault {0}

	int read() {
		return values[cur]
	}

	void write(int v){
		values[cur] = v
	}

	void left() {
		--cur
	}

	void right(){
		++cur
	}

	int value(){
		return values.values().sum()
	}
}

String state = 'A'

int steps = 12656374

Tape tape = new Tape()

steps.times {
	if(it % 100000 == 0){
		println "Iter ${it+1}/$steps"
	}
	switch(state) {
		case 'A': 
			if(tape.read() == 0){
				tape.write(1)
				tape.right()
				state = 'B'
			}else{
				tape.write(0)
				tape.left()
				state = 'C'
			}
			break
		case 'B': 
			if(tape.read() == 0){
				tape.write(1)
				tape.left()
				state = 'A'
			}else{
				tape.write(1)
				tape.left()
				state = 'D'
			}
			break
		case 'C': 
			if(tape.read() == 0){
				tape.write(1)
				tape.right()
				state = 'D'
			}else{
				tape.write(0)
				tape.right()
				state = 'C'
			}
			break
		case 'D': 
			if(tape.read() == 0){
				tape.write(0)
				tape.left()
				state = 'B'
			}else{
				tape.write(0)
				tape.right()
				state = 'E'
			}
			break
		case 'E': 
			if(tape.read() == 0){
				tape.write(1)
				tape.right()
				state = 'C'
			}else{
				tape.write(1)
				tape.left()
				state = 'F'
			}
			break
		case 'F': 
			if(tape.read() == 0){
				tape.write(1)
				tape.left()
				state = 'E'
			}else{
				tape.write(1)
				tape.right()
				state = 'A'
			}
			break
	}
}

println tape.value()
