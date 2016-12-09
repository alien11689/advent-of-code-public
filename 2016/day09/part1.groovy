def main(String input) {
    new File(input).text
        .split('\n')
	.collect {println it; decompress(it) }
        .collect {println it; it.size()}
        .sum()
}

def decompress(String text){
	List<String> response = []
	for(int i =0; i < text.size(); ){
		if(text[i] == '('){
			int ruleEnd = text.indexOf(')', i + 1)
			if(ruleEnd == -1) {
				throw new Exception()	
			}
			int chars = text[i..ruleEnd].split(/[\(x\)]/)[1] as int 
			int repeat = text[i..ruleEnd].split(/[\(x\)]/)[2] as int
			String newText = text[(ruleEnd + 1)..(ruleEnd + chars)]
			repeat.times {
				response << newText
			}
			i = ruleEnd + chars +1
		}else {
			response << text[i]
			++i
		}
	}
	response.join('')
}

def isRule(String text){
	text.contains('x')	
}

println(main('sample.txt'))
println()
println(main('input.txt'))
