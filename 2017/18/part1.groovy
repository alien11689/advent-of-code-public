String text = new File('input.txt').text
//text = new File('input2.txt').text
List lines = text.split('\n').collect {it.trim()}

