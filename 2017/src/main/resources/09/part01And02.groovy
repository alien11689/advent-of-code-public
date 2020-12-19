def text = new File('input.txt').text.trim()

int score = 0
int sum = 0
boolean garbage = false
int garbageAmount = 0

for(int i = 0; i < text.size(); ++i){
	switch(text[i]){
		case "!": 
			++i
			break
		case "<":
			if(!garbage){
				garbage = true
			}else {
				garbageAmount++
			}
			break
		case "{":
			if (!garbage){
				++score	
				sum += score
			}else{
				++garbageAmount
			}
			break
		case ">": 
			garbage = false
			break
		case "}":
			if(!garbage){
				--score
			}else{
				++garbageAmount
			}
			break
			
		default: 
			if(garbage){
				garbageAmount++
			}
			break
	}
}
println "Score sum = $sum"
println "Garbage amount = $garbageAmount"
