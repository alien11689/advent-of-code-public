def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

int amount = 0

lines.each{ line ->
	List words = line.split(/\s/)
	println words
	if(words.size() == new HashSet(words).size()){
		++amount
	}
}
println amount
