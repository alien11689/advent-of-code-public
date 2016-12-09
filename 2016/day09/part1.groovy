def main(String input) {
    new File(input).text
        .split('\n')
	.collect {println it; decompress(it) }
        .collect {println it; it.size()}
        .sum()
}

def decompress(String text){
	def split = text.split(/[\(\)]/).findAll().collectMany {isRule(it) ? ["($it)"] : it.toCharArray().collect {it as String}}
	List<String> newText = []
	for(int i = 0; i < split.size(); ++i){
		println "Split: ${split[i]}"
		if(isRule(split[i])){
			def ruleSplit = split[i].split(/[\(x\)]/)
			def howMany = ruleSplit[2] as int
			def chars = ruleSplit[1]  as int
			howMany.times {
				println (split[(i+1)..-1].collectMany{println it; (it.toCharArray() as List<String>) })
				newText.addAll(split[(i+1)..-1].collectMany{(it.toCharArray() as List<String>) }
					[0..<chars]
					.collectMany {it as String})	
			}
		}else{
			newText << split[i]
		}	
	}
	newText.join('')
}

def isRule(String text){
	text.contains('x')	
}

println(main('sample.txt'))
println()
//println(main('input.txt'))
