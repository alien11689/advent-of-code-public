String text = new File('input.txt').text.trim()
int row = 25
int tall = 6

(text as List).collate(row * tall).each { println it.size() }

def sol = (text as List).collate(row * tall).min { it.findAll { it == '0' }.size() }
// not 2142
println(sol.findAll { it == '1' }.size() * sol.findAll { it == '2' }.size())
