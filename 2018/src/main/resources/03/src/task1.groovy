import groovy.transform.ToString

def text = new File('input.txt').text
int size = 1000

def lines = text.split('\n')

@ToString
class Carpet {
    String id;
    int fromLeft
    int fromTop
    int width;
    int height
}

List<Carpet> carpets = lines.collect {
    def parts = it.split(/[ #@,:x]/)
    new Carpet(id: parts[1], fromLeft: parts[4] as int,
            fromTop: parts[5] as int, width: parts[7] as int,
            height: parts[8] as int)
}

@ToString
class Cell {
    Set ids = [] as Set

    def contains(String id) {
        id in ids
    }
}

List<List<Cell>> field = (1..size).collect { (1..size).collect { new Cell() } }
carpets.each {
    (it.fromTop..(it.fromTop + it.height - 1)).each { i ->
        (it.fromLeft..(it.fromLeft + it.width - 1)).each { j ->
            field[i][j].ids << it.id
        }
    }
}
println(field.flatten().findAll { it.ids.size() > 1 }.size())