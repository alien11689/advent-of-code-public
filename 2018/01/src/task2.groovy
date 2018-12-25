def text = new File('input.txt').text

def numbers = text.split('\n').collect { it as int }

def m = [] as Set
def start = 0
try {
    while (true) {
        start = numbers.inject(start) { acc, cur ->
            def next = acc + cur
            if (next in m) {
                println next
                throw new RuntimeException()
            }
            m << next
            next
        }
    }
} catch (Exception e) {
}
