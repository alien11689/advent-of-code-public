def text = new File('input.txt').text.trim()
int half = text.size() / 2

def sum = 0

for(int i = 0; i < text.size(); ++i){
	int next = (i + half) % text.size()
	if(text[i] == text[next]) {
		sum += (text[i] as int)
	}
}
println sum
