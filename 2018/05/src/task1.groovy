def text = new File('input.txt').text.trim()

def before = text
def iteration = 0
def curSize = text.size()
def nextSize = 0

def changes = ('a'..'z').collectMany { [/$it${it.toUpperCase()}/, /${it.toUpperCase()}$it/] }

while (curSize != nextSize) {
    curSize = nextSize
//    println("Iteration: ${iteration++} - size ${curSize}")
    changes.each {
        before = before.replaceAll(it, '')
    }
    nextSize = before.size()
}

//println(before)
println(nextSize)
