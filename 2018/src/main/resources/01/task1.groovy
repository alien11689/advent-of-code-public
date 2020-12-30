def text = new File('input.txt').text

def sum = text.split('\n').collect { it as int }.sum()

println sum
