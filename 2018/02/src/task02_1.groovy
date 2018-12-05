def text = new File('input.txt').text

def lines = text.split('\n')

int count2 = 0
int count3 = 0

lines.each { line ->
	Map<String, Integer> m = [:]
	line.each { letter ->
		m[letter] = m.getOrDefault(letter, 0) + 1
	}
	count2 += (2 in m.values())? 1:0
	count3 += (3 in m.values())? 1:0
	
}

println(count2*count3)
