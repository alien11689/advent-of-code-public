int input = 344
//input = 3

int amount = 2017

List buf = [0]
int cur = 0

(1..2017).each {i -> 
	int curInput = input % buf.size
	while(curInput > 0){
		cur = (cur + 1) % buf.size
		--curInput
	}
	buf.add(++cur, i)
}
println buf[cur + 1]
