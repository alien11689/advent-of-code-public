def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

int amount = 0

lines.each{ line ->
	List words = line.split(/\s/)
	List wordsAsSets = words.collect {(it as List).sort().join('')}
	println words
	if(words.size() == new HashSet(wordsAsSets).size()){
		++amount
	}
}
println amount
