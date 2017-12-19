String text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

