int input = 344
//input = 3

int amount = 50000000

int size = 1
int cur = 0
int nextToZero = -1

(1..amount).each {i ->
	if(i%10000 == 0){
		println i
	} 
	int curInput = input % size
	cur = (cur + curInput) % size
	++cur
	++size
	if(cur == 1) {
		nextToZero = i 
	}
}
println nextToZero
