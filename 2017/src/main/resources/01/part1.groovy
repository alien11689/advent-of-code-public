def text = new File('input.txt').text.trim()

println text.size()

def sum = 0

for(int i = 0; i < text.size(); ++i){
	def next = (i + 1) % text.size()
	if(text[i] == text[next]) {
		sum += (text[i] as int)
	}
}
println sum
