def text = new File('input.txt').text.trim()

changes = ('a'..'z').collectMany { [/$it${it.toUpperCase()}/, /${it.toUpperCase()}$it/] }

reducting = ('a'..'z').collect { /[$it${it.toUpperCase()}]/ }


def reduce(text) {
    def iteration = 0
    def before = text
    def curSize = text.size()
    def nextSize = 0
    while (curSize != nextSize) {
        curSize = nextSize
        println("Iteration: ${iteration++} - size ${curSize}")
        changes.each {
            before = before.replaceAll(it, '')
        }
        nextSize = before.size()
    }
    return before
}

def result = reduce(text)

println(reducting.collect { println("Reducing $it"); reduce(result.replaceAll(it, '')) }.min{it.size()}.size())
